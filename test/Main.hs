import Data.Metric.String (Hamming(..), Levenshtein(..), RestrictedDamerauLevenshtein(..))
import Data.Metric.Vector.Real (Discrete(..), Euclidean(..), Taxicab(..), Cosine(..), Chebyshev(..), PostOffice(..))
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

instance Arbitrary Discrete where
  arbitrary = Discrete <$> arbitrary 

prop_DiscreteMetric :: Discrete -> Discrete -> Discrete -> Property
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

-- | Cosine

instance Arbitrary Cosine where
  arbitrary = oneof [choose (1,1000), choose (-1,-1000)]
          >>= vectorOf 3 . return . fromRational . (%100)
          >>= return . Cosine . fromList

prop_CosineMetric :: Cosine -> Cosine -> Cosine -> Property
prop_CosineMetric = prop_Metric

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
  , testProperty "cosine"      $ prop_CosineMetric
  , testProperty "chebyshev"   $ prop_ChebyshevMetric
  , testProperty "postoffice"  $ prop_PostOfficeMetric
  , testProperty "hamming"     $ prop_HammingMetric
  , testProperty "levenshtein" $ prop_LevenshteinMetric
  ]
