module Common.Types where

type Identifier = String

infix 1 <!>
(<!>) :: Maybe a -> b -> Either b a
(<!>) (Just a) _ = Right a
(<!>) Nothing err = Left err

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right b) = Just b
