type BookId = Int
type Title = String
type Authors = [String]

--data Book = Book BookId Title Authors deriving (Show)
data Book = Book { bookId  :: BookId,
                   title   :: Title,
                   authors :: Authors } deriving (Show)

myBook = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerId = Int
type ReviewBody = String

data BookReview = BookReview Book CustomerId ReviewBody
                  deriving (Show)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery 
                 | Invoice CustomerId
                   deriving (Show)
