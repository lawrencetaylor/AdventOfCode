using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;

namespace CSharpAdvent
{
    public static class Day06
    {
        public static Square Parse(string str, string keyWord)
        {
            var testString = str.Substring(str.IndexOf(keyWord) + keyWord.Length + 1);
            var firstPass = testString.Split(',');
            var x1 = int.Parse(firstPass[0]);
            var y2 = int.Parse(firstPass[2]);

            var secondParse = firstPass[1].Replace("through", ",").Split(',');
            var y1 = int.Parse(secondParse[0]);
            var x2 = int.Parse(secondParse[1]);

            return new Square()
            {
                startX = x1,
                startY = y1,
                endX = x2,
                endY = y2
            };
        }

        public static Tuple<int, Square> P(string s)
        {
            if (s.Contains("on")) return Tuple.Create(1, Parse(s, "on"));
            if (s.Contains("off")) return Tuple.Create(2, Parse(s, "off"));
            if (s.Contains("toggle")) return Tuple.Create(3, Parse(s, "toggle"));

            return null;
        }

        public static void Apply(Tuple<int, Square> e, Dictionary<Point, int> lights)
        {
            for(int i = e.Item2.startX ; i <= e.Item2.endX; i++)
                for(int j = e.Item2.startY; j <= e.Item2.endY; j++)
                    switch (e.Item1)
                    {
                        case 1:
                            lights[new Point(i, j)] = lights[new Point(i, j)] + 1;
                            continue;
                        case 2:
                            lights[new Point(i, j)] = Math.Max(lights[new Point(i, j)] - 1, 0);
                            continue;
                        case 3:
                            lights[new Point(i, j)] = lights[new Point(i, j)] + 2;
                            continue;
                    }
        }

        public static int solution()
        {
            var x = File.ReadAllLines(@"../../../AdventOfCode/Day06.txt");
            Tuple<int, Square>[] enumerable = x.Select(P).ToArray();

            var initialLights = Enumerable.Range(0, 1000);

            Dictionary<Point, int> lightMap = new Dictionary<Point, int>();
            foreach (var xLight in initialLights)
            {
                foreach (var yLight in initialLights)
                {
                    lightMap.Add(new Point(xLight, yLight), 0 );
                }
            }

            var count = lightMap.Select(kvp => kvp.Value).Sum();

            foreach (var tuple in enumerable)
            {
                Apply(tuple, lightMap);
                count = lightMap.Select(kvp => kvp.Value).Sum();
            }

            count = lightMap.Select(kvp => kvp.Value).Sum();
            

            return 2;
        }

        public class Square
        {
            public int startX { get; set; }
            public int startY { get;set; }

            public int endX { get; set; }
            public int endY { get; set; }
        }
    }
}
