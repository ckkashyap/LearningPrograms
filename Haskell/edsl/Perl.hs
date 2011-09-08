data PerlVal a = PerlString a | PerlNumber a

instance (Show a) => Show (PerlVal a) where
         show (PerlString a) = show a
         show (PerlNumber a) = show a

data PerlVar =
     PerlStringVar String 
     | PerlNumberVar String 
--     | PerlStringArrayVar String (PerlVal [PerlVal String])
--     | PerlNumberArrayVar String (PerlVal [PerlVal Double])

instance Show PerlVar where
         show (PerlStringVar name) = "$" ++ name
         show (PerlNumberVar name) = "$" ++ name
   
data Action = 
     Print PerlVar
     | AssignString PerlVar (PerlVal String)
     | AssignNumber PerlVar (PerlVal Double)

instance Show Action where
         show (Print a) = "print " ++ (show a) ++ ";"
         show (AssignString a b) = (show a) ++ " = " ++ (show b) ++ ";"
         show (AssignNumber a b) = (show a) ++ " = " ++ (show b) ++ ";"


data Perl = Perl [Action]

instance Show Perl where
         show (Perl [])  = ""
         show (Perl (x:xs)) = (show x) ++ "\n" ++ (show (Perl xs))


v1=PerlString "Hello"
n1=PerlStringVar "hello"
example = Perl [AssignString n1 v1]

