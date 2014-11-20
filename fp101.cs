using System;

namespace CSharp
{
	class MainClass
	{
		public static void Main ()
		{
			Console.WriteLine ("Hello World!");

			Func<int, int> f = x => x * 2;

			Func<int, int> h = (x) => {
				Console.WriteLine ("...");
				return x * 2;
			};

			Console.WriteLine (f (3));

			Console.WriteLine (h (3));

			Expression<Func<int, int>> g = x => x * 2;

			Console.WriteLine (g);
		}
	}
}

