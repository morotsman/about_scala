package generics.java.contravariance;


import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public class ContravarianceJava {

	public static void main(String... args) {
		Eatable eatable;
		Apple apple = new Apple("Red");
		eatable = apple; //works since apple is a subtype of eatable
		// apple = eatable;

		// how to use a Consumer
		howToUseAConsumer();

		// invariance for a Consumer
		invariantConsumer();

		// covariance?
		covariantConsumer();

		// contravariant
		contravariantConsumer();
	}

	private static void howToUseAConsumer() {
		Consumer<Apple> myConsumer = new Consumer<Apple>() {
			@Override
			public void accept(Apple apple) {
				System.out.println("I like: " + apple.color() + " apples!");
			}
		};
		myConsumer.accept(new Apple("Yellow"));

		Consumer<Apple> myConsumer1 = (Apple a) -> System.out.println("I like: " + a.color() + " apples!");
		myConsumer1.accept(new Apple("Red"));

		Consumer<Apple> myConsumer2 = a -> System.out.println("I like: " + a.color() + " apples!");
		myConsumer2.accept(new Apple("Green"));
	}

	private static void invariantConsumer() {
		Consumer<Eatable> eatableConsumer = (Eatable e) -> System.out.println("With this fruit I can produce: " + e.joice());
		getEatableFruits(eatableConsumer);
	}

	private static void getEatableFruits(Consumer<Eatable> eatableConsumer) {
		eatableConsumer.accept(new Apple("Red"));
	}

	private static void covariantConsumer() {
		Consumer<? extends Eatable> eatableConsumerCovariant;
		Consumer<Apple> appleConsumer = (Apple a) -> System.out.println("With this fruit I can: " + a.removeAppleCore());
		eatableConsumerCovariant = appleConsumer;
		getEatableFruitsCovariant(eatableConsumerCovariant);
	}

	private static void getEatableFruitsCovariant(Consumer<? extends Eatable> eatableConsumer) {
		Orange orange = new Orange();
		// eatableConsumer.accept(orange); // will not compile

		Apple apple = new Apple("Red");
		// eatableConsumer.accept(apple); // will not compile
	}

	private static void contravariantConsumer() {
		Fruit fruit;
		Eatable eatable = new Apple("Red");
		fruit = eatable; //works since eatable is a subtype of fruit

		Consumer<? super Eatable> eatableConsumerContravariant = (Eatable e) -> System.out.println("With this fruit I can produce: " + e.joice());
		getEatableFruitsContravariant(eatableConsumerContravariant);

		Consumer<Fruit> fruitConsumer = (Fruit f) -> System.out.println("The color of the fruit is: " + f.color());
		getEatableFruitsContravariant(fruitConsumer);
	}

	private static void getEatableFruitsContravariant(Consumer<? super Eatable> eatableConsumer) {
		Orange orange = new Orange();
		eatableConsumer.accept(orange);

		Apple apple = new Apple("Red");
		eatableConsumer.accept(apple);

		Fruit fruit = new Fruit() {
			@Override
			public String color() {
				return "Red";
			}
		};
		// eatableConsumer.accept(fruit); // will not compile
	}
}
