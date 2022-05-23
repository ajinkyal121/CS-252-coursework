> import Control.Applicative
> import qualified Data.Set as Set

Faceted values are an **information flow mechanism**.  Specifically, they are
designed to store differing views of data.  For more technical details, see
"Multiple Facets for Dynamic Information Flow", available at
https://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf.

Authorized viewers should see the real value, and other viewers should see
dummy data instead.  To do this, we need to represent the security level
of a piece of data.  We'll define labels to encapsulate this information,
represented as strings.

> type Label = String

A user may have many security privileges, so a "view" of a faceted value can
be represented as a set of labels.

> type View = Set.Set Label

A faceted value can be either a raw (that is, unfaceted) value, or it can
be node containing two nested faceted values, with a label tracking who is
allowed to view the contents.

> data FacetedValue a = Raw a
>                     | Facet Label (FacetedValue a) (FacetedValue a)

Note that we do **not** derive Show.  Instead, code must pass its
authorizations to a view function, which will return a non-faceted value.
If any label is not in the set, it is assumed that the view is not authorized.

> view :: View -> FacetedValue a -> a
> view _ (Raw x) = x
> view labels (Facet k auth unauth) =
>     if Set.member k labels then
>       view labels auth
>     else
>       view labels unauth

We can apply operations to a faceted value, in which case the action should
be applied to every element of the tree.  As a review of the last lab, define
fmap for FacetedValues.

> instance Functor FacetedValue where
>   fmap f (Raw a) = Raw (f a)
>   fmap f (Facet a b c) = Facet a (fmap f b) (fmap f c)  


The following function gives an example of how a FacetedValue can be used.
In this case, a customer's Visa credit card is hidden from other viewers.
If someone with other permissions, say with the ability to view details
about the customer's Mastercard, they will instead be presented with the
default view of 0.  Even if someone tries to do some calculations on the
credit card in the hope of revealing information, a consistent view will be
presented to the observer.

> testFmap = do
>   let creditCard = Facet "visa" (Raw 4111111111111111) (Raw 0)
>       ccPlusOne = fmap (+1) creditCard
>   putStrLn "Credit card views:"
>   print $ view (Set.fromList ["visa"]) ccPlusOne -- Should print 4111111111111112
>   print $ view (Set.fromList ["mastercard"]) ccPlusOne -- Should print 1
>   print $ view (Set.fromList []) ccPlusOne -- Should print 1



While this works, what happens if we want two faceted values to be combined?
In order to make that work, we need need to add support for Applicative Functors.
Define the behavior of the Functor below

> instance Applicative FacetedValue where
>   pure = Raw  
>   (Raw f) <*> v =  fmap f v
>   (Facet a b c) <*> v = Facet a (b <*> v) (c <*> v)


The code below gives an example of how this might come up.  If code authorized
to read your Bank of America account details runs on the same machine as code
to read your Wells Fargo details, **neither** will be allowed to read your
combined balance.  However, you as the customer can see the true values.

> testApplicative = do
>   let bofaBalance = Facet "Bank of America" (Raw 44) (Raw 0)
>       wellsFargoBalance = Facet "Wells Fargo" (Raw 122) (Raw 0)
>       combinedBalance = (+) <$> bofaBalance <*> wellsFargoBalance
>   print $ view (Set.fromList []) combinedBalance -- Should print 0
>   print $ view (Set.fromList ["Bank of America"]) combinedBalance -- Should print 44
>   print $ view (Set.fromList ["Wells Fargo"]) combinedBalance -- Should print 122
>   print $ view (Set.fromList ["Bank of America", "Wells Fargo"]) combinedBalance -- Should print 166
