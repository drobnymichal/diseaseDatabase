import Data.List

type PersonId = Integer
type PersonName = String
type InfectionId = Integer
type InfectionName = String

data TerminationType = Recovered | Dead
                       deriving (Eq, Show)

data Person = Person PersonId PersonName
              deriving Show

data Infection = Infection InfectionId PersonId InfectionName
                 deriving Show

data Termination = Termination InfectionId TerminationType
                   deriving Show

type Persons = [Person]
type Infections = [Infection]
type Terminations = [Termination]


infName :: Infection -> InfectionName
infName (Infection _ _ name) = name

infPerson :: Infection -> PersonId
infPerson (Infection _ id _) = id 

infId :: Infection -> PersonId
infId (Infection id _ _) = id 

termType :: Termination -> TerminationType
termType (Termination _ termType) = termType

termId :: Termination -> InfectionId
termId (Termination id _) = id

personName :: Person -> PersonName
personName (Person _ name) = name 

personId :: Person -> PersonId
personId (Person id _) = id


countOfInfected :: Infections -> InfectionName -> Int
countOfInfected x name = (length.nub.map g.filter f) x
       where  f x = name == infName x
              g x = infPerson x

dead :: Terminations -> [InfectionId]
dead x = map g (filter f x)
       where f x = termType x == Dead
             g x = termId x

activeCases :: Infections -> Terminations -> Infections
activeCases x y = filter (g (map f y)) x
       where f y = termId y 
             g y x = infId x `notElem` y

somebodyDied :: Infections -> Terminations -> [InfectionName]   
somebodyDied x y = nub (map infName (filter (g (map termId (filter f y))) x)) 
       where f x = termType x == Dead
             g y x = infId x `elem` y 
         
checkDeaths :: Infections -> Terminations -> Bool
checkDeaths x y = count (map infPerson (filter (f (map termId (filter (\x -> termType x == Dead) y))) x)) 
       where f y x = infId x `elem` y
             count x = length x == length (nub x)

diseases :: Persons -> Infections -> [(PersonName, [InfectionName])]
diseases x y = zip (map personName x) (map (f y) x)
       where f y x = map infName (filter (g x) y)
             g x y = personId x == infPerson y


pers :: Persons
pers = [Person 1 "Augustin"
       ,Person 2 "Baltazar"
       ,Person 42 "Ctirad"
       ,Person 128 "Zdenek"
       ,Person 5 "Drahoslav"
       ]

infs :: Infections
infs = [Infection 2020 1 "COVID"
       ,Infection 2019 42 "COVID"
       ,Infection 1 5 "COVID"
       ,Infection 5 128 "rymicka"
       ,Infection 3 5 "astma"
       ,Infection 2 1 "astma"
       ,Infection 128 5 "zapal plic"
       ]

ters :: Terminations
ters = [Termination 2020 Dead
       ,Termination 2 Recovered
       ,Termination 2019 Recovered
       ,Termination 128 Dead
       ]
