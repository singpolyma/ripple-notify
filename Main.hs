module Main (main) where

import Data.String (fromString)
import Data.Word (Word32)
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Error (readMay, runMaybeT, MaybeT(..), hoistMaybe)
import Data.Base58Address (RippleAddress)
import Text.Email.Validate (EmailAddress, emailAddress)
import Network.Mail.Mime (Address(..), Mail(..), Part(..), Encoding(..), renderSendMail)
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

wsSendJSON :: (WS.TextProtocol p, Aeson.ToJSON j) => j -> WS.WebSockets p ()
wsSendJSON = WS.sendTextData . Aeson.encode

wsReceiveJSON :: (WS.TextProtocol p, Aeson.FromJSON j) => WS.WebSockets p (Maybe j)
wsReceiveJSON = fmap Aeson.decode WS.receiveData

plainMail :: EmailAddress -> EmailAddress -> String -> String -> Mail
plainMail to from subject body = Mail {
		mailFrom = emailToAddress from,
		mailTo = [emailToAddress to],
		mailCc = [],
		mailBcc = [],
		mailHeaders = [(fromString "Subject", T.pack subject)],
		mailParts = [[
			Part (T.pack "text/plain; charset=utf-8") QuotedPrintableText Nothing [] (TL.encodeUtf8 $ TL.pack body)
		]]
	}
	where
	emailToAddress = Address Nothing . T.pack . show

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"

data TransactionType = PaymentTransaction
	deriving (Show, Eq)

instance Aeson.FromJSON TransactionType where
	parseJSON (Aeson.String s)
		| s == T.pack "Payment" = return PaymentTransaction
	parseJSON _ = fail "Only support TransactionType \"Payment\""

data Currency = XRP | Currency (Char,Char,Char) RippleAddress
	deriving (Eq)

instance Show Currency where
	show XRP = "XRP"
	show (Currency (a,b,c) adr) = [a,b,c,'/'] ++ show adr

data Amount = Amount Rational Currency
	deriving (Eq)

instance Show Amount where
	show (Amount a c) =
		show (realToFrac a :: Double) ++ "/" ++ show c

instance Aeson.FromJSON Amount where
	parseJSON (Aeson.String t) =
		case readMay s :: Maybe Integer of
			Nothing -> case readMay s :: Maybe Double of
				Nothing -> fail "Could not parse XRP amount"
				Just d -> return $ Amount (realToFrac d) XRP
			Just i -> return $ Amount (realToFrac i / 1000000) XRP
		where
		s = T.unpack t
	parseJSON (Aeson.Object o) = Amount <$>
			(parseToRational =<< (Aeson..:) o (T.pack "value")) <*>
			(Currency <$>
				(str3 =<< (Aeson..:) o (T.pack "currency")) <*>
				(readM =<< (Aeson..:) o (T.pack "issuer"))
			)
		where
		parseToRational s = case readMay s :: Maybe Double of
			Just d -> return (realToFrac d :: Rational)
			Nothing -> fail "Amount value is not a valid number"
		str3 [a,b,c] = return (a,b,c)
		str3 _ = fail "Currency code must be three letters"
	parseJSON _ = fail "Amount is always string or object in JSON"

newtype DestinationTag = DestinationTag Word32
	deriving (Show, Eq)

instance Aeson.FromJSON DestinationTag where
	parseJSON (Aeson.Number i) =
		return $ DestinationTag (floor i)
	parseJSON _ = fail "DestinationTag is 32bit integer"

data Transaction = Transaction {
		ttype   :: TransactionType,
		tfrom   :: RippleAddress,
		tto     :: RippleAddress,
		tamount :: Amount,
		tdt     :: Maybe DestinationTag
	} deriving (Show, Eq)

instance Aeson.FromJSON Transaction where
	parseJSON (Aeson.Object o) = Transaction              <$>
			(Aeson..:) o (T.pack "TransactionType")         <*>
			(readM =<< (Aeson..:) o (T.pack "Account"))     <*>
			(readM =<< (Aeson..:) o (T.pack "Destination")) <*>
			(Aeson..:) o (T.pack "Amount")                  <*>
			(Aeson..:?) o (T.pack "DestinationTag")
	parseJSON _ = fail "Transaction is always a JSON object"

newtype TransactionMessage = TransactionMessage Transaction
	deriving (Show, Eq)

instance Aeson.FromJSON TransactionMessage where
	parseJSON (Aeson.Object o) =
		-- TODO: also, "type": "transaction"
		TransactionMessage <$> (Aeson..:) o (T.pack "transaction")
	parseJSON _ = fail "TransactionMessage is always a JSON object"

data Commands =
	SubscribeAccounts [RippleAddress]
	deriving (Show, Eq)

instance Aeson.ToJSON Commands where
	toJSON (SubscribeAccounts adrs) = Aeson.object [
			(Aeson..=) (T.pack "command") (T.pack "subscribe"),
			(Aeson..=) (T.pack "accounts") (map (T.pack . show) adrs)
		]

loop :: EmailAddress -> [(RippleAddress,EmailAddress)] -> WS.WebSockets WS.Hybi10 ()
loop appEmail adrs = forever $ runMaybeT $ do
	TransactionMessage transaction <- MaybeT wsReceiveJSON
	email <- hoistMaybe $ lookup (tto transaction) adrs
	let mail = plainMail email appEmail
		("Transaction from " ++ show (tfrom transaction))
		(
			"Amount: " ++ show (tamount transaction) ++
			"\nSending account: " ++ show (tfrom transaction) ++
			"\nReceiving account: " ++ show (tto transaction) ++
			maybe "" (\(DestinationTag i) -> "\nDestination Tag: " ++ show i) (tdt transaction)
		)
	liftIO $ renderSendMail mail

start :: EmailAddress -> [(RippleAddress,EmailAddress)] -> WS.WebSockets WS.Hybi10 ()
start appEmail adrs = do
	liftIO $ putStrLn "Connected..."
	wsSendJSON (SubscribeAccounts $ map fst adrs)
	loop appEmail adrs

main :: IO ()
main =
	-- TODO: command line arguments and/or data file
	WS.connect "s1.ripple.com" 51233 "/" (start email [
		(read "r3ADD8kXSUKHd6zTCKfnKT3zV9EZHjzp1S", email)
	])
	where
	email = fromJust $ emailAddress $ fromString "notify@rippleunion.com"
