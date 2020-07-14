package generics.java.covariance;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import static java.util.stream.Collectors.*;


import static java.util.Arrays.asList;

public class CovarianceJava {



	public static void main(String... args) {
		covariantListExample();

		arrayExample();

		supplierExample();
	}

	private static void covariantListExample() {
		Fruit fruit;
		Apple apple = new Apple("Red");
		fruit = apple; //works since apple is a subtype of fruit
		fruit.color();
		// apple = fruit;

		List<Fruit> fruits;
		List<Apple> apples = new ArrayList<>(asList(new Apple("Red"), new Apple("Green")));
		// fruits = apples; // will not compile

		List<? extends Fruit> fruitsCovariant;
		fruitsCovariant = apples;
		Fruit someFruit = fruitsCovariant.get(0);


		printColorOfFruit(fruit);
		printColorOfFruit(apple);


		List<Fruit> listOfFruit = new ArrayList<>(asList(new Apple("Red"), new Apple("Green"), new Orange(), new Orange()));
		List<Apple> listOfApple = new ArrayList<>(asList(new Apple("Red"), new Apple("Green")));

		System.out.println(groupFruitByColor(listOfFruit));
		// System.out.println(groupFruitByColor(listOfApple)); // will not compile
		System.out.println(groupAppleByColor(listOfApple));

		// listOfFruit = listOfApple; // will not compile
		System.out.println(groupFruitByColorCovariant(listOfFruit));
		System.out.println(groupFruitByColorCovariant(listOfApple));


		// not possible to add elements to the List

		List<? extends Fruit> listOfFruitCovariant = new ArrayList<>(asList(new Apple("Red"), new Apple("Green")));
		listOfFruitCovariant = listOfApple;
		listOfApple.add(new Apple("Green"));
		Fruit myFruit = listOfFruit.get(0);
		Apple myApple = listOfApple.get(0);
		// listOfFruitCovariant.add(new Apple("Red")); // dangerous operation, not allowed
		// listOfFruitCovariant.add(new Orange()); // dangerous operation, not allowed

		Fruit aFruit = listOfFruitCovariant.get(0); // This is ok
	}

	private static void printColorOfFruit(Fruit fruit) {
		System.out.printf("The color is: %s%n", fruit.color());
	}

	private static Map<String, List<Fruit>> groupFruitByColor(List<Fruit> fruits) {
		return fruits.stream().collect(groupingBy(Fruit::color));
	}

	private static Map<String, List<Apple>> groupAppleByColor(List<Apple> fruits) {
		return fruits.stream().collect(groupingBy(Apple::color));
	}

	private static Map<String, List<Fruit>> groupFruitByColorCovariant(List<? extends Fruit> fruits) {
		return fruits.stream().collect(groupingBy(Fruit::color));
	}

	private static void arrayExample() {
		// possible to add elements to Array
		Fruit[] arrayOfFruit;
		Apple[] arrayOfApple = new Apple[]{new Apple("Red"), new Apple("Green")};
		arrayOfFruit = arrayOfApple;
		// arrayOfApple[0] = new Orange(); // will not compile
		try {
			arrayOfFruit[0] = new Orange(); // Exception in runtime
		} catch (ArrayStoreException e){
			System.out.println(e.toString());
		}
		Arrays.stream(arrayOfApple).forEach(a -> System.out.println(a.joice()));
	}

	private static void supplierExample() {
		System.out.println("Supplier example: ");

		// invariance for a Supplier
		Supplier<Fruit> supplierOfFruit = new Supplier<Fruit>() {
			@Override
			public Fruit get() {
				return new Apple("Red");
			}
		};
		Supplier<Apple> supplierOfApple = () -> new Apple("Green");
		// supplierOfFruit = supplierOfApple; will not compile
		Apple apple = supplierOfApple.get();
		Fruit alsoFruit = supplierOfApple.get();




		printColorOfApple(supplierOfApple);
		printColorOfFruit(supplierOfFruit);
		printColorOfFruit(() -> new Apple("Red"));
		// printColorOfFruit(supplierOfApple); // will not compile


		// covariance for a Supplier
		Supplier<? extends Fruit> supplierOfFruitCovariant;
		supplierOfFruitCovariant = supplierOfApple;
		Fruit fruit = supplierOfFruitCovariant.get();

		printColorOfFruitCovariant(supplierOfFruit);
		printColorOfFruitCovariant(supplierOfApple);
	}

	private static void printColorOfFruit(Supplier<Fruit> supplier) {
		printColorOfFruit(supplier.get());
	}

	private static void printColorOfApple(Supplier<Apple> supplier) {
		printColorOfFruit(supplier.get());
	}

	private static void printColorOfFruitCovariant(Supplier<? extends Fruit> supplier) {
		printColorOfFruit(supplier.get());
	}

}
