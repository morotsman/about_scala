package generics.java.contravariance;

interface Fruit {
	String color();
};

interface Eatable extends Fruit {
	String joice();
}

class Orange implements Eatable {
	public String color() {
		return "Orange";
	}

	@Override
	public String joice() {
		return "Orange joice";
	}
}

class Apple implements Eatable {
	private final String color;

	public Apple(String color) {
		this.color = color;
	}

	@Override
	public String color() {
		return color;
	}

	@Override
	public String joice() {
		return "Apple joice";
	}

	public String removeAppleCore() {
		return "Apple core removed";
	}
}


