package com.flowjo.plugin.ezDAFi.utilities;

        import java.io.IOException;
        import java.net.ServerSocket;
        import java.util.concurrent.ThreadLocalRandom;

public class PortUtils {

    public static int nextFreePort(int from, int to) {
        int port = ThreadLocalRandom.current().nextInt(from, to + 1);
        while (true) {
            if (isLocalPortFree(port)) {
                return port;
            } else {
                port = ThreadLocalRandom.current().nextInt(from, to);
            }
        }
    }

    private static boolean isLocalPortFree(int port) {
        try {
            new ServerSocket(port).close();
            return true;
        } catch (IOException e) {
            return false;
        }
    }
}
