import Debug.Trace
import Data.Char
import List

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Eq, Ord, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
    Jack | Queen | King | Ace
    deriving (Eq, Ord, Show)

data Card = Card {
    cardVal :: Value,
    cardSuit :: Suit
    } deriving (Eq, Ord, Show)


takeValues ::  [Card] -> [Value]
takeValues = map cardVal

takeSuits ::  [Card] -> [Suit]
takeSuits = map cardSuit

sortBySuits ::  [Card] -> [Card]
sortBySuits = sortBy (\x -> \y -> compare (cardSuit x) (cardSuit y))

sortByValues ::  [Card] -> [Card]
sortByValues = sortBy (\x -> \y -> compare (cardVal x) (cardVal y))

parseValues ::  String -> [Value]
parseValues = takeValues . parse_deck

make_card2 ::  Char -> Char -> Card
make_card2 v s = Card (parse_value v) (parse_suit s)
    where
        parse_value v =
            case (toUpper v) of
                '2' -> Two
                '3' -> Three
                '4' -> Four
                '5' -> Five
                '6' -> Six
                '7' -> Seven
                '8' -> Eight
                '9' -> Nine
                'T' -> Ten
                'J' -> Jack
                'Q' -> Queen
                'K' -> King
                'A' -> Ace
                _ -> error (v : ": unknown value")
        parse_suit s = 
            case (toUpper s) of
                 'S' -> Spades
                 'H' -> Hearts
                 'D' -> Diamonds
                 'C' -> Clubs
                 _ -> error (s : ": unknown suit")


make_card ::  String -> Card
make_card (v:t:[]) = make_card2 v t
make_card _ = error "make_card requires a two-char string"


format_card ::  Card -> String
format_card (Card v s) = (format_value v):(format_suit s):[]
    where
        format_value v = case v of
                              Two -> '2'
                              Three -> '3'
                              Four -> '4'
                              Five -> '5'
                              Six -> '6'
                              Seven -> '7'
                              Eight -> '8'
                              Nine -> '9'
                              Ten -> 'T'
                              Jack -> 'J'
                              Queen -> 'Q'
                              King -> 'K'
                              Ace -> 'A'
                            
        format_suit s = case s of
                             Spades -> 'S'
                             Hearts -> 'H'
                             Diamonds -> 'D'
                             Clubs -> 'C'

parse_deck ::  String -> [Card]
parse_deck s = map make_card (words s)


data PokerHand =
    HighCard Value
    | Pair Value
    | TwoPair Value Value
    | ThreeOfAKind Value
    | Straight Value
    | Flush Value
    | FullHouse Value Value
    | FourOfAKind Value
    | StraightFlush Value
    deriving (Show, Eq, Ord)


-- requires input sorted by value
check_pair ::  Eq a => [a] -> Maybe a
check_pair (a:b:[])
    | a == b  = Just a
    | otherwise = Nothing
check_pair (a:b:xs)
    | a == b =
        case check_pair (b:xs) of
             Just c -> Just c
             Nothing -> Just a
    | otherwise = check_pair (b:xs)
                            


groupFilterByLengthMin n xs = filter (\x -> n <= length x) (group xs)
groupFilterByLengthMinMax m n xs =
    filter (\x -> let len = length x in
                      n <= len && m >= len)
           (group xs)

-- requires input sorted by value
check_two_pair ::  Eq a => [a] -> Maybe (a, a)
check_two_pair (a:b:c:d:e:[])
    | a == b && c == d = Just (a, c)
    | a == b && d == e = Just (a, d)
    | b == c && d == e = Just (b, d)
    | otherwise = Nothing
check_two_pair xs =
    let best = (take 2 . reverse . groupFilterByLengthMin 2) xs in
        case best of
             ((a:as):(b:bs):[]) -> Just (b, a)
             _ -> Nothing


-- requires input sorted by value
check_full_house ::  Eq a => [a] -> Maybe (a, a)
check_full_house (a:b:c:d:e:[])
    | a == b && b == c && d == e = Just (a, d)
    | a == b && c == d && d == e = Just (c, a)
    | otherwise = Nothing

check_full_house xs =
    let g = (reverse . groupFilterByLengthMin 2) xs in
        cfh g Nothing Nothing
        where
            cfh [] (Just a) (Just b) = Just (a, b)
            cfh [] _ _ = Nothing
            cfh (_:[]) (Just a) (Just b) = Just (a, b)
            cfh (_:[]) _ _ = Nothing
            cfh (a:b:[]) Nothing Nothing
                | length a > 2 = Just (head a, head b)
                | length b > 2 = Just (head b, head a)
                | otherwise = Nothing
            cfh (a:b:[]) (Just c3) Nothing = Just (c3, head a)
            cfh (a:b:[]) Nothing (Just c2)
                | length a > 2 = Just (head a, c2)
                | length b > 2 = Just (head b, c2)
                | otherwise = Nothing
            cfh (a:xs) Nothing Nothing
                | length a > 2 = cfh xs (Just (head a)) Nothing
                | otherwise = cfh xs Nothing (Just (head a))
            cfh (a:xs) (Just c3) Nothing = Just (c3, head a)
            cfh (a:xs) Nothing (Just c2)
                | length a > 2 = Just (head a, c2)
                | otherwise = cfh xs Nothing (Just c2)
            cfh _ (Just c3) (Just c2) = Just (c3, c2)


-- requires input sorted by value
check_three_of_a_kind ::  Eq a => [a] -> Maybe a
check_three_of_a_kind (a:b:c:[])
    | a == b && b == c = Just a
    | otherwise = Nothing
check_three_of_a_kind (a:b:c:xs)
    | a == b && b == c =
        case check_three_of_a_kind (b:c:xs) of
             Just c -> Just c
             Nothing -> Just a
    | otherwise = check_three_of_a_kind (b:c:xs)
check_three_of_a_kind _ = Nothing


-- requires input sorted by value
check_four_of_a_kind ::  Eq a => [a] -> Maybe a
check_four_of_a_kind (a:b:c:d:[])
    | a == b && b == c && c == d = Just a
    | otherwise = Nothing
check_four_of_a_kind (a:b:c:d:xs)
    | a == b && b == c && c == d =
        case check_four_of_a_kind (b:c:d:xs) of
             Just c -> Just c
             Nothing -> Just a
    | otherwise = check_four_of_a_kind (b:c:d:xs)
check_four_of_a_kind _ = Nothing



-- requires input sorted by value
check_straight ::  [Value] -> Maybe Value
check_straight (a:b:c:d:e:[]) =
    check a b c d e
    where
        check Ace Two Three Four Five = Just Five
        check Two Three Four Five Six = Just Six
        check Three Four Five Six Seven = Just Seven
        check Four Five Six Seven Eight = Just Eight
        check Five Six Seven Eight Nine = Just Nine
        check Six Seven Eight Nine Ten = Just Ten
        check Seven Eight Nine Ten Jack = Just Jack
        check Eight Nine Ten Jack Queen = Just Queen
        check Nine Ten Jack Queen King = Just King
        check Ten Jack Queen King Ace = Just Ace
        check _ _ _ _ _ = Nothing

check_straight xs =
    case _check_straight xs of
         Just e -> Just e
         Nothing -> let last = take 1 (reverse xs) in
                        case last of
                             Ace:_ -> _check_straight (Ace:xs)
                             _ -> Nothing
    where
        _check_straight (a:b:c:d:e:xs) =
            let g = group (a:b:c:d:e:xs) in
                case g of
                     (a':_):(b':_):(c':_):(d':_):(e':_):_ ->
                        case check_straight (a':b':c':d':e':[]) of
                            Just e' ->
                                case _check_straight (b:c:d:e:xs) of
                                     Just f -> Just f
                                     Nothing -> Just e'
                            Nothing -> _check_straight (b:c:d:e:xs)
                     _ -> Nothing 

        _check_straight _ = Nothing

-- requires input sorted by suit, value
check_flush ::  [Card] -> Maybe Value
check_flush xs =
    let g = groupBy (\x -> \y -> (cardSuit x) == (cardSuit y)) xs in
        case filter (\x -> length x >= 5) (map reverse g) of
             [] -> Nothing
             (x:xs):_ -> Just (cardVal x)


-- requires input sorted by suit
check_straight_flush ::  [Card] -> Maybe Value
check_straight_flush (a:b:c:d:e:[])
    | (cardSuit a) == (cardSuit b) && (cardSuit b) == (cardSuit c)
        && (cardSuit c) == (cardSuit d) && (cardSuit d) == (cardSuit e) =
            check_straight (sort (takeValues (a:b:c:d:e:[])))
    | otherwise = Nothing
check_straight_flush (a:b:c:d:e:xs) =
    case check_straight_flush (a:b:c:d:e:[]) of
         Just card -> case check_straight_flush (b:c:d:e:xs) of
                           Just card2 -> Just card2
                           Nothing -> Just card
         Nothing -> check_straight_flush (b:c:d:e:xs)

check_straight_flush _ = Nothing


getHand cards =
    case check_straight_flush cards of
         Just card -> StraightFlush card
         Nothing ->
             case check_four_of_a_kind ((sort . takeValues ) (cards)) of
                  Just card -> FourOfAKind card
                  Nothing ->
                      case check_full_house ((sort . takeValues) (cards)) of
                           Just (card3, card2) -> FullHouse card3 card2
                           Nothing ->
                               case check_flush (sortBySuits (sortByValues cards)) of
                                    Just card -> Flush card
                                    Nothing ->
                                        case check_straight ((sort . takeValues) (cards))of
                                             Just card -> Straight card
                                             Nothing ->
                                                 check_three_and_less cards
    where
        check_three_and_less cards =
            case check_three_of_a_kind ((sort . takeValues) (cards)) of
                 Just card -> ThreeOfAKind card
                 Nothing ->
                     case check_two_pair ((sort . takeValues) (cards)) of
                          Just (hicard, locard) -> TwoPair hicard locard
                          Nothing ->
                              case check_pair ((sort . takeValues) (cards)) of
                                   Just card -> Pair card
                                   Nothing ->
                                       HighCard ((head . reverse . sort . takeValues) (cards))
