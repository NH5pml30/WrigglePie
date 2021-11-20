public class Sum {
    private static int sumString(String s) {
        int res = 0, i = 0, len = s.length();
        while (i < len) {
            while (i < len && Character.isWhitespace(s.charAt(i))) {
                i++;
            }
            int begin = i;
            while (i < len && !Character.isWhitespace(s.charAt(i))) {
                i++;
            }
            if (i > begin) {
                res += Integer.parseInt(s.substring(begin, i));
            }
        }
        return res;
    }

    public static void main(String[] args) {
        int res = 0;
        for (String s : args) {
            res += sumString(s);
        }
        System.out.println(res);
    }
}