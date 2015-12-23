using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace CSharpAdvent
{
    public static class Day08
    {
        public static string StripSingleQuotes(string str)
        {
            return str.Replace("\"", "~");
        }

        public static string StripBackSlash(string str)
        {
            return str.Replace("\\\\", "#");
        }

        public static string regStr = @"\x(.)(.)";

        public static string StripHex(string str)
        {
            return null;
        }

        public static int solution()
        {
            var x = File.ReadAllLines(@"../../../AdventOfCode/Day08.txt");
            var unformattedlength = x.Select(s => s.Length).Sum();

            var formattedLength = x.Select(StripSingleQuotes).Select(StripBackSlash).Select(StripHex)
                .Select(s => s.Length)
                .Sum();

            var diff = unformattedlength - formattedLength;

            return 2;
        }


    }
}