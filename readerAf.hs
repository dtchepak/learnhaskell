fn :: (r -> a -> b) -> (r -> a) -> (r -> b)
fn f fa = \r -> f r (fa r)
