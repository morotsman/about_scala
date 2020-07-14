package generics.java.covariance;

interface Fruit {
	String color();
};

class Orange implements Fruit {
	public String color() {
		return "Orange";
	}
}

class Apple implements Fruit {
	private final String color;

	public Apple(String color) {
		this.color = color;
	}

	@Override
	public String color() {
		return color;
	}

	public String joice() {
		return "Apple joice";
	}
}


