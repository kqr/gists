Moduldeklaration
----------------

För att deklarera att "detta är en modul" är syntaxen

    module Vehicle where

som ska stå högst upp i källkodsfilen som utgör modulen `Vehicle`.



Modulimportering
----------------

För att importera en annan modul, kan man använda något av

    import Data.Functor
    import qualified Data.Set as Set

Den första gör alla exporterade funktioner i `Data.Functor` tillgängliga
i nuvarande namespace. Den andra importerar alla funktioner, men med
prefixet `Set`, så till exempel för att anropa `size` måste man skriva
`Set.size`.

För att importera enbart specifika funktioner från bibliotek gör man på
ungefär samma sätt som när man exporterar specifika funktionen.

    import Data.List (intersperse, groupBy)

importerar bara funktionerna `intersperse` och `groupBy` från
`Data.List`-modulen.



Primitiva datatyper
-------------------

Det finns några datatyper som är "primitiva", där man mer eller mindre
måste lita på att språket tar hand om dem. De är

 * `Integer`
 * `Double`
 * `Char`

En `Integer` skrivs som en siffra, till exempel `17`. En `Double` skrivs
som en siffra med decimalpunkt, till exempel `1.414`. En `Char` skrivs
inom enkelfnuttar, till exempel `'K'`.

Strängar är bara listor av `Char`s. Listor och booleska datatyper kan
med lätthet definieras i standardbiblioteket.


Typdefinitioner
---------------

För att definiera en ny datatyp är syntaxen

    data Boolesk = Sann | Falsk

denna deklaration definierar en ny typ `Boolesk`, som kan anta värdena
`Sann` eller `Falsk`. Denna typ motsvarar `Bool`-typen i standardbiblioteket.

Typer och deras konstruktorer kan även ta argument, som i följande
definition som definierar en länkad lista.

    data List a = Cons a (List a)
                | Nil

Här är typen `List a`, för alla `a`:n, och listan är antingen ett `a`-
värde consat med en lista, eller `Nil` som markerar slutet på listan. Man
kan skapa listor från denna typ som

    Cons 1 (Cons 2 (Cons 3 Nil))

Detta lär vara sånt du känner igen från Lisp. Denna typ motsvarar List-typen
i standardbiblioteket, som använder lite specialsyntax men fungerar likadant.

Datatyper och deras konstruktorer börjar med stor bokstav, till skillnad från
vanliga funktioner.



Typsignaturer
-------------

Varje värde och uttryck kan ha en typsignatur kopplat till sig. Värdet skiljs
från typsignaturen med dubbelkolon. Till exempel är följande giltiga värden:

    3 :: Integer
    3.14 :: Double
    Nil :: List Integer
    Cons 1 (Cons 2 Nil) :: List Integer
    sqrt 9.0 :: Double

Även funktioner har typsignaturer, och där skiljs funktionsargumenten åt med
en pil. Sista typen (till höger om sista pilen) är returvärdets typ. Följande
typsignaturer för funktioner är giltiga

    sqrt :: Double -> Double
    car :: List a -> a
    ite :: Boolesk -> a -> a -> a

där den sista motsvarar en if-then-else-sats. Om `Boolesk`-värdet är sant
returnerar den sitt första argument, annars det andra.

Ibland har funktioner typklasser kopplade till sin signatur, till exempel

    weakIf :: Truthy b => b -> a -> a -> a

Detta är en variant av `ite`-funktionen, fast den tar inte bara `Boolesk`-
värden som första argument, utan vilken typ som helst som är medlem i `Truthy`!
(Se längre ned för vad `Truthy` är.)

Konstruktorer har också en typsignatur, ex.vis

    Cons :: a -> List a -> List a



Funktionsapplikation
--------------------

Funktioner appliceras på argument genom att skriva först funktionens namn
och sedan argumenten, allt separerat med mellanslag. Om ett argument innehåller
mellanslag så får man sätta parenteser runt det. Till exempel

    sqrt 9.0
    ite Falsk 1 0
    car (Cons 3 (Cons 17 Nil))

Den första returnerar 3.0, den andra 0, och den sista 3.

Operatorer är funktioner vars namn består av specialtecken. Operatorer tar två
argument, och själva operatorn skrivs mellan argumenten. Exempel inkluderar

    3 + 5
    Cons 3 Nil ++ Cons 4 Nil

där `++` är operatorn för att slå ihop två listor.




Funktionskonstruktion
---------------------

För att konstruera funktioner används lambda-syntax. En funktion som tar en
`Integer` och returnerar samma `Integer` fast plus `1` kan till exempel
definieras som

    \i -> i + 1

där det är tänkt att det första backslashet ska se ut som ett lambda-tecken.

Flera argument kan uppnås genom att bara skriva dem efter varandra mellan
backslashet och pilen. Till exempel en funktion som tar tre argument och
summerar deras kvadratrötter kan skrivas som

    \a b c -> sqrt a + sqrt b + sqrt c


Variabeldefinition
------------------

Värden kan tilldelas namn med likamed-tecknet. Till exempel

    pi = 3.14159

eller

    succ = \i -> i + 1



Branching
---------

Man måste också ha någon form av branching. För det finns `case ... of`-
uttrycket. Syntaxen är exempelvis

    case siffra of
      1 -> 1
      2 -> 4
      3 -> 9
      i -> i*i

där om siffra är 1, returnerar `case`-uttrycket 1 och så vidare. Om siffran
inte är 1, 2, eller 3 så binds den till variabeln `i` och `case`-uttrycket
returnerar `i*i`.

`case`-uttryck kan även matcha på konstruktorer, som i följande exempel:

    case lista of
      Cons i (Cons j Nil) -> i + j
      Cons i (Cons j _)   -> 0 - i - j
      _                   -> 0

Här matchar man på en lista, och om listan har längd 2 returneras summan av
de två talen i listan. Om listan är längre än 2 (understreck står för "don't
care" och matchar allt) returneras negativa summan av de två första talen.
Om listan är kortare än 2 returneras 0.

`case`-uttryck kan naturligtvis användas för att implementera till exempel
`car`-funktionen:

    \lista ->
      case lista of
        Cons a _ -> a

Här kraschar programmet om den ges en tom lista, eftersom vi inte angav vad
som ska hända i det fallet. Om den ges en lista med minst ett värde så
returneras det värdet.

Det går nu även att implementera `ite`-funktionen.

    \bool then else ->
      case bool of
        Sant   -> then
        Falskt -> else

Här returneras `then`-argumentet om `bool` har värdet `Sant`, annars
returneras `else`-argumentet.



Typklassdefinitioner
--------------------

Typklasser definieras som

    class Truthy a where
      tillBool :: a -> Boolesk

Detta definierar en typklass `Truthy` för typer som kan konverteras till ett booleskt
värde. Den allmänna formen för en typklassdefinition är första raden som bestämmer
vad typklassen heter, och sen en lista av funktioner som typer måste stödja för
att få vara medlemmar i klassen.

I det här fallet måste alla medlemmar i `Truthy` implementera funktionen `tillBool`.



Instansdeklarationer
--------------------

För att göra en typ medlem i en typklass måste man ge en implementation för funktionerna
typklassen kräver. Vår `Truthy`-klass kräver bara en funktion, så vi kan implementera
den för vår `List`-typ. Vi vill att `Nil` ska räknas som `Falskt`, och resten ska räknas
som `Sant`.

    instance Truthy (List a) where
      tillBool = \lista ->
        case lista of
          Nil -> Falskt
          _   -> Sant

Vi kan även föreställa oss en typ för värden som kanske inte existerar.

    data Kanske a = Finns a
                  | Saknas

Värden av typen `Kanske Integer` till exempel kan antingen vara något i stil med `Finns 17`
eller så är det `Saknas`, vilket indikerar att det inte finns något värde. Om vi vill göra
denna typ en medlem i `Truthy` kan vi tänka oss att `Saknas` alltid ska räknas som `Falskt`,
men `Finns a` ska bara räknas som `Sant` om `a` räknas som `Sant`! Detta implementeras som

    instance Truthy a => Kanske a where
      tillBool = \kanske ->
        case kanske of
          Saknas -> Falskt
          Finns a -> tillBool a

där vi har angett "förkravet" att `Kanske a` kan bara vara medlem i `Truthy` om `a` också
är det.
