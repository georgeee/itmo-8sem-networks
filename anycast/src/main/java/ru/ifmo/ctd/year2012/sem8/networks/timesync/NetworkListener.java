package ru.ifmo.ctd.year2012.sem8.networks.timesync;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.nio.ByteBuffer;

public class NetworkListener extends Thread {
    private static final Logger log = LoggerFactory.getLogger(ConsoleListener.class);
    private final int port;

    public NetworkListener(int port) {
        this.port = port;
    }

    @Override
    public void run() {
        try (DatagramSocket socket = new DatagramSocket(port)) {
            while (true) {
                byte[] inData = new byte[69];
                DatagramPacket inPacket = new DatagramPacket(inData, inData.length);
                socket.receive(inPacket);
                String time = new String(inPacket.getData()).trim();
                if ("time".equals(time)) {
                    ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES);
                    buffer.putLong(System.currentTimeMillis());
                    byte[] outData = buffer.array();
                    DatagramPacket outPacket = new DatagramPacket(outData, outData.length, inPacket.getAddress(), inPacket.getPort());
                    socket.send(outPacket);
                    System.out.println("Packet sent");
                }
            }
        } catch (IOException e) {
            log.error("Console listener exception", e);
        }
    }
}
