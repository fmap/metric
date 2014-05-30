import Data.Metric.Set (Discrete(..))
import Data.Metric.String (Hamming(..), Levenshtein(..), RestrictedDamerauLevenshtein(..))
import Data.Metric.Vector.Real (Euclidean(..), Taxicab(..), Chebyshev(..), PostOffice(..))
import Control.Applicative ((<$>))
import Data.Vector (Vector(..), fromList)
import Data.Ratio ((%))
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), conjoin, vector, vectorOf, oneof, choose)
import Test.QuickCheck.Property.Metric (prop_Metric)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = fromList <$> vector 2

-- | Discrete

instance (Eq a, Arbitrary a) => Arbitrary (Discrete a) where
  arbitrary = Discrete <$> arbitrary

prop_DiscreteMetric :: Discrete Char -> Discrete Char -> Discrete Char -> Property
prop_DiscreteMetric = prop_Metric

-- | Euclidean

instance Arbitrary Euclidean where
  arbitrary = Euclidean <$> arbitrary 

prop_EuclideanMetric :: Euclidean -> Euclidean -> Euclidean -> Property
prop_EuclideanMetric = prop_Metric

-- | Taxicab

instance Arbitrary Taxicab where
  arbitrary = Taxicab <$> arbitrary 

prop_TaxicabMetric :: Taxicab -> Taxicab -> Taxicab -> Property
prop_TaxicabMetric = prop_Metric

-- | Chebyshev

instance Arbitrary Chebyshev where
  arbitrary = Chebyshev <$> arbitrary 

prop_ChebyshevMetric :: Chebyshev -> Chebyshev -> Chebyshev -> Property
prop_ChebyshevMetric = prop_Metric

-- | PostOffice

instance Arbitrary PostOffice where
  arbitrary = PostOffice <$> arbitrary 

prop_PostOfficeMetric :: PostOffice -> PostOffice -> PostOffice -> Property
prop_PostOfficeMetric = prop_Metric

-- | Hamming

instance Arbitrary Hamming where
  arbitrary = Hamming <$> vector 10

prop_HammingMetric :: Hamming -> Hamming -> Hamming -> Property
prop_HammingMetric = prop_Metric

-- | Levenshtein

instance Arbitrary Levenshtein where
  arbitrary = Levenshtein <$> arbitrary 

prop_LevenshteinMetric :: Levenshtein -> Levenshtein -> Levenshtein -> Property
prop_LevenshteinMetric = prop_Metric

-- | Restricted Damerau Levenshtein

instance Arbitrary RestrictedDamerauLevenshtein where
  arbitrary = RestrictedDamerauLevenshtein <$> arbitrary 

prop_RestrictedDamerauLevenshteinMetric :: RestrictedDamerauLevenshtein -> RestrictedDamerauLevenshtein -> RestrictedDamerauLevenshtein -> Property
prop_RestrictedDamerauLevenshteinMetric = prop_Metric

-- |

main :: IO ()
main = defaultMain . return . testGroup "QuickCheck" $
  [ testProperty "discrete"    $ prop_DiscreteMetric
  , testProperty "euclidean"   $ prop_EuclideanMetric
  , testProperty "taxicab"     $ prop_TaxicabMetric
  , testProperty "chebyshev"   $ prop_ChebyshevMetric
  , testProperty "postoffice"  $ prop_PostOfficeMetric
  , testProperty "hamming"     $ prop_HammingMetric
  , testProperty "levenshtein" $ prop_LevenshteinMetric
  ]
