module Genesis

NamePath : Type
NamePath = List String

data CoreCommand : Type where
  --this needs to be private so that only genesis can run it
  AlterCommand : {type : Type} -> (namePath : NamePath) -> (value : type) -> CoreCommand

Coins : Type  
Coins = Integer

PubKey : Type
PubKey = Int

UpdateIndex : Type
UpdateIndex = Integer

--note that the address is the reference to the account, ex. 'timsAccount'
record Account where
  constructor MkAccount
  bal : Coins
  pubKey : PubKey
  updInd : UpdateIndex -- increments for every update

timsAccount : Account
timsAccount = MkAccount 100 123456789101112 0

ccsAccount : Account
ccsAccount = MkAccount 50 1122334455 0

--TODO we need to make a compiler time function that translates a name
-- to a reference
--a reference to an object
data Ref : Type -> Type where
  MkRef : NamePath -> (t: Type) -> Ref t



--TODO
--This should return the value of a given reference, for example 'timsAccount'
--If the type doesn't match, a compiler error will result
getRefValue : Ref type -> type
  
Signature : Type  
Signature = Int  
   
--This verifies a transfer funds signature is valid
--The signature should sign
-- * fromName
-- * toName
-- * amt
-- * from updInd
-- TODO 3 the updInd prevents a txn from being reused multiple times, but 
--  it limits one txn per from account per block unless the miner can order
--  the txns (which is a fair amount of work)
verifySigTransferFunds : (sig : Signature) -> (from : Ref Account) -> (to : Ref account) -> (amt : Coins) -> Bool
verifySigTransferFunds _ _ _ _ = True -- TODO 2 implement

-- verifies no negative balance
verifySufficientFunds : Account -> Coins -> Bool
verifySufficientFunds acct amt = True -- (bal acct) >= amt

transferFunds : (from : Ref Account) -> (to : Ref Account) -> (amt : Coins) -> (sig : Signature) -> verifySigTransferFunds sig from to amt = True -> verifySufficientFunds (getRefValue from) amt = True -> List CoreCommand  --TODO 2 CoreCommand should probably be a monad
transferFunds from to amt _ _ _ = 
  let 
    fromAccount = getRefValue from
    toAccount = getRefValue to
    (MkRef fromName _) = from
    (MkRef toName _) = to
    newFromAccount = record { bal = (bal fromAccount) - amt, updInd $= (+1) } fromAccount
    newToAccount = record { bal = (bal toAccount) - amt, updInd $= (+1) } toAccount
    in
      [ AlterCommand fromName newFromAccount
        ,AlterCommand fromName newToAccount ]

--TODO 2 to create new accounts, transfer funds to them
--transferFundsToNewAccount : (from : Ref Account) -> (to : Ref Account) -> (amt : Coins) -> (sig : Signature) -> verifySigTransferFunds sig from to amt = True -> verifySufficientFunds (getRefValue from) amt = True -> List CoreCommand  --TODO 2 CoreCommand should probably be a monad
-- transferFunds from to amt _ _ _ = 
--   let 
--     fromAccount = getRefValue from
--     toAccount = getRefValue to
--     (MkRef fromName _) = from
--     (MkRef toName _) = to
--     newFromAccount = record { bal = (bal fromAccount) - amt, updInd $= (+1) } fromAccount
--     newToAccount = record { bal = (bal toAccount) - amt, updInd $= (+1) } toAccount
--     in
--       [ AlterCommand fromName newFromAccount
--         ,AlterCommand fromName newToAccount ]

        
-- --   [ AlterCommand fromName Account (MkAccount ((bal (getRefValue fromName)) - amt) (
 
--pretend block txn
lunchMoney : List CoreCommand
lunchMoney = transferFunds (MkRef [ "timsAccount" ] Account) 
                           (MkRef [ "ccsAccount" ] Account)
                           10
                           0
                           Refl
                           Refl
                           
 
