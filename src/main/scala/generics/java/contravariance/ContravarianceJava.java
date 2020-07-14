package generics.java.contravariance;


import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;

public class ContravarianceJava {

	private static void providerOfAppleInvariant(Consumer<Apple> consumerOfApple) {
		consumerOfApple.accept(new Apple("Red"));
	}

	private static void providerOfAppleContravariant(Consumer<? super Apple> consumerOfApple) {
		consumerOfApple.accept(new Apple("Red"));
	}

	public static void main(String... args) {
		Fruit fruit;
		Apple apple = new Apple("Red");
		fruit = apple; //works since apple is a subtype of fruit
		// apple = fruit;


		// invariance for a Consumer
		Consumer<Apple> consumerOfApple = (Apple a) -> System.out.println("I executed: " + a.removeAppleCores());
		Consumer<Fruit> consumerOfFruit = (Fruit f) -> System.out.println("My color is: " + f.color());

		consumerOfFruit.accept(apple);
		consumerOfFruit.accept(fruit);
		consumerOfApple.accept(apple);
		//consumerOfApple.accept(fruit); // will not compile

		// providerOfAppleInvariant(consumerOfFruit); // will not compile
		providerOfAppleInvariant(consumerOfApple);

		// consumerOfApple = consumerOfFruit; // will not compile
		// consumerOfFruit = consumerOfApple; // will not compile


		// contravariance for a Consumer
		System.out.println("Contravariance");
		Consumer<? super Apple> consumerOfAppleContravariant = (Apple a) -> System.out.println(a.joice());

		// consumerOfFruit = consumerOfApple;
		consumerOfAppleContravariant = consumerOfFruit; // works since consumerOfApple is a subtype of consumerOfGoldenDelicious

		providerOfAppleContravariant(consumerOfFruit);


		// ************************************************************************************

		// invariant function

		Function<Ackee, Optional<? extends Eatable>> ackeeDetoxifier = Ackee::detoxify;
		Function<Toxic, Optional<? extends Eatable>> genericDetoxifier = t -> {
			System.out.println("Generic detoxify");
			return t.detoxify();
		};

		// ackeeDetoxifier = genericDetoxifier

		// contravariant/covariant
		Function<? super Ackee, ? extends Optional<? extends Eatable>> ackeeDetoxifier2 = Ackee::detoxify;
		ackeeDetoxifier2 = genericDetoxifier;


		new Ackee().detoxify(); // better example needed?

		genericDetoxifier
				.apply(new Ackee())
				.ifPresent(e -> System.out.println("Got: " + e.joice()));

	}

}
