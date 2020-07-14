package generics.java.covariance;

interface FruitCovariant {
	Joice getJoice();
};

class OrangeCovariant implements FruitCovariant {
	/* will not compile
	@Override
	public Object getJoice() {
		return new OrangeJoice();
	}
	 */

	@Override
	public OrangeJoice getJoice() {
		return new OrangeJoice();
	}
}

class AppleCovariant implements FruitCovariant {
	@Override
	public Joice getJoice() {
		return new AppleJoice();
	}
}

interface Joice {
}

class AppleJoice implements Joice {
}

class OrangeJoice implements Joice {
}


