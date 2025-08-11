#[derive(Default, Debug, Clone)]
pub struct Array<T> {
	data: Vec<T>,
}

impl<T> Array<T> {
	pub fn new() -> Self {
		Self { data: Vec::new() }
	}

	pub fn len(&self) -> usize {
		self.data.len()
	}

	pub fn capacity(&self) -> usize {
		self.data.capacity()
	}

	pub fn is_empty(&self) -> bool {
		self.data.is_empty()
	}

	pub fn iter(&self) -> std::slice::Iter<'_, T> {
		self.data.iter()
	}

	pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
		self.data.iter_mut()
	}

	pub fn push(&mut self, value: T) {
		self.data.push(value);
	}
}

impl<T> From<Vec<T>> for Array<T> {
	fn from(vec: Vec<T>) -> Self {
		Array { data: vec }
	}
}

impl<T> std::ops::Index<usize> for Array<T> {
	type Output = T;

	fn index(&self, index: usize) -> &Self::Output {
		&self.data[index]
	}
}

impl<T> std::ops::IndexMut<usize> for Array<T> {
	fn index_mut(&mut self, index: usize) -> &mut Self::Output {
		&mut self.data[index]
	}
}

impl<'a, T> IntoIterator for &'a Array<T> {
	type Item = &'a T;
	type IntoIter = std::slice::Iter<'a, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter()
	}
}

impl<'a, T> IntoIterator for &'a mut Array<T> {
	type Item = &'a mut T;
	type IntoIter = std::slice::IterMut<'a, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.iter_mut()
	}
}

impl<T> FromIterator<T> for Array<T> {
	fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Array<T> {
		Self {
			data: Vec::from_iter(iter),
		}
	}
}
