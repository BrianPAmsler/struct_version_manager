pub trait ConvertTo<T: Sized> {
    fn convert_to(self) -> T;
}