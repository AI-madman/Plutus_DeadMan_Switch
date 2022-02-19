{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module DeadMan_Switch where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import Data.Bool (Bool (False))
import Distribution.Simple.Command (OptDescr(BoolOpt))

data DeadParams=DeadParams
    { beneficiary :: paymentPubKeyHash,
      owner :: paymentPubKeyHash,
      deadline ::  Integer
} deriving Show 

PlutusTx.makeLift ''DeadParams

{-# MAKEINLEABLE mkValidator #-}

mkValidator :: DeadParams -> () -> () -> ScriptContext -> Bool
mkValidator p _ _ ctx = traceIfFalse "Sending Wallet Sig Missing" chkSig &&
                        traceIfFalse "The Deadline has not been Reached" chkDeadline 
    where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        chkSig :: Bool 
        chkSig = txSignedBy info $ unpaymentPubKeyHash $ owner p 

        chkDeadline :: Bool
        chkDeadline = False

data DeadParams
instance Scripts.ValidatorTypes DeadParams where 
    type instance DatumType DeadParams = ()
    type instance RedeemerType DeadParams = ()

typedValidator :: DeadParams -> Scripts.TypedValidator DeadParams
typedValidator p = Scripts.mkTypedValidator @DeadParams
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
  $$(PlutusTx.compile [|| wrap ||]) where
    wrap =Scripts.wrapValidator @() @()

validator :: DeadParams -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: DeadParams -> Ledger.ValidatorHash
valHash = Scripts.ValidatorHash . typedValidator

scrAddress :: DeadParams -> Ledger.Address
scrAddress = scriptAdress . validator 