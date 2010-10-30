myTrue = \x -> \y -> x
myFalse = \x -> \y -> y


myIf = \b -> \m -> \n -> b m n


myNot = \x -> (myIf x myFalse myTrue)
