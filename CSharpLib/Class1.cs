using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Policy;
using System.Text;
using System.Text.RegularExpressions;

namespace CSharpLib
{
    public static class HexHelpers
    {
        public static byte[] FromHex(string hex)
        {
            hex = hex.Replace("-", "");
            byte[] raw = new byte[hex.Length / 2];
            for (int i = 0; i < raw.Length; i++)
            {
                raw[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
            }
            return raw;
        }

        public static string fromHex(string hex)
        {
            return Encoding.ASCII.GetString(FromHex(hex));
        }
    }

    public static class D16
    {
        public static IEnumerable<string> Day16(string[] inputs)
        {
            return Common16(inputs)
                .Where(i => !i.Contains("pomeranians:") || i.Contains("pomeranians: 3"))
                .Where(i => !i.Contains("trees:") || i.Contains("trees: 3"))
                .Where(i => !i.Contains("goldfish:") || i.Contains("goldfish: 5"))
                .Where(i => !i.Contains("cats:") || i.Contains("cats: 7"));
        }

        public static IEnumerable<string> Day16Part2(string[] inputs)
        {
            return Common16(inputs)
                .Where(i => !i.Contains("pomeranians:") || Regex.IsMatch(i, "pomeranians: [012]"))
                .Where(i => !i.Contains("trees:") || Regex.IsMatch(i, "trees: [4-9]"))
                .Where(i => !i.Contains("goldfish:") || Regex.IsMatch(i, "goldfish: [0-4]"))
                .Where(i => !i.Contains("cats:") || Regex.IsMatch(i, "cats: [89]"));
        }

        private static IEnumerable<string> Common16(string[] inputs)
        {
            return inputs
                .Select(i => Regex.Replace(i, ": \\d\\d", ": 9"))
                .Where(i => !i.Contains("akitas:") || i.Contains("akitas: 0"))
                .Where(i => !i.Contains("vizslas:") || i.Contains("vizslas: 0"))
                .Where(i => !i.Contains("perfumes:") || i.Contains("perfumes: 1"))
                .Where(i => !i.Contains("cars:") || i.Contains("cars: 2"))
                .Where(i => !i.Contains("samoyeds:") || i.Contains("samoyeds: 2"))
                .Where(i => !i.Contains("children:") || i.Contains("children: 3"));
        }
    }
}
