pub trait FileFormat {
	fn open(path: &std::path::Path) -> Option<Self>
	where
		Self: Sized;

	fn save(&mut self, path: &std::path::Path) -> std::io::Result<()>;
}
