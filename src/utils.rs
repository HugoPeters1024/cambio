use num_traits::{One, Zero};
use std::ops::AddAssign;

pub struct Seq<T>
where
    T: Copy + AddAssign + One,
{
    current: T,
}

impl<T: Copy + Zero + One + AddAssign> Default for Seq<T> {
    fn default() -> Self {
        Seq { current: T::zero() }
    }
}

impl<T> Seq<T>
where
    T: Copy + AddAssign + One + Zero,
{
    pub fn generate(&mut self) -> T {
        let value = self.current;
        self.current += T::one();
        value
    }
}
