package generics.java.contravariance;

import java.util.Optional;

interface Fruit {
	String color();
};

interface Eatable extends Fruit {
	String joice();
};

interface Toxic extends Fruit {
	Optional<? extends Eatable> detoxify();
};

class Orange implements Eatable {
	public String joice() {
		return "Orange joice";
	}

	public String color() {
		return "Orange";
	}

	@Override
	public String toString() {
		return "Orange{}";
	}
}

class Apple implements Eatable {
	private final String color;

	public Apple(String color) {
		this.color = color;
	}

	public String joice() {
		return "Apple joice";
	}

	public String removeAppleCores() {
		return "Apple cores removed";
	}

	@Override
	public String color() {
		return color;
	}

	@Override
	public String toString() {
		return "Apple{" +
				"color='" + color + '\'' +
				'}';
	}
}

class ProcessedAckee implements Eatable {
	public String joice() {
		return "Ackee joice";
	}

	@Override
	public String color() {
		return "Red";
	}

	@Override
	public String toString() {
		return "ProcessedAckee{}";
	}
}

class Ackee implements Toxic {
	public String color() {
		return "Red";
	}

	public Optional<ProcessedAckee> detoxify() {
		return Optional.of(new ProcessedAckee());
	}

	@Override
	public String toString() {
		return "Ackee{}";
	}
}
