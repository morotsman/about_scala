package generics.java.contravariance;


import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public class ContravarianceJava {

	private static void getEatableFruits(Consumer<Eatable> eatableConsumer) {
		eatableConsumer.accept(new Apple("Red"));
	}

	private static void getEatableFruitsCovariant(Consumer<? extends Eatable> eatableConsumer) {
		Orange orange = new Orange();
		// eatableConsumer.accept(orange); // will not compile

		Apple apple = new Apple("Red");
		// eatableConsumer.accept(apple); // will not compile
	}

	private static void getEatableFruitsContravariant(Consumer<? super Eatable> eatableConsumer) {
		Orange orange = new Orange();
		eatableConsumer.accept(orange);

		Apple apple = new Apple("Red");
		eatableConsumer.accept(apple);
	}


	public static void main(String... args) {
		Eatable eatable;
		Apple apple = new Apple("Red");
		eatable = apple; //works since apple is a subtype of eatable
		// apple = eatable;

		// invariance for a Consumer
		Consumer<Eatable> eatableConsumer = (Eatable e) -> System.out.println("With this fruit I can produce: " + e.joice());
		getEatableFruits(eatableConsumer);

		// covariance?
		Consumer<? extends Eatable> eatableConsumerCovariant;
		Consumer<Apple> appleConsumer = (Apple a) -> System.out.println("With this fruit I can: " + a.removeAppleCore());
		eatableConsumerCovariant = appleConsumer;
		getEatableFruitsCovariant(eatableConsumerCovariant);

		// contravariant
		Consumer<? super Eatable> eatableConsumerContravariant;
		Consumer<Fruit> fruitConsumer = (Fruit f) -> System.out.println("The color of the fruit is: " + f.color());
		eatableConsumerContravariant = fruitConsumer;
		getEatableFruitsContravariant(eatableConsumerContravariant);


	}

}
