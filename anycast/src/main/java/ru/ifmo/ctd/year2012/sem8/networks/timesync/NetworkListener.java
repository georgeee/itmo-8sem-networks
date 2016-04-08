package ru.ifmo.ctd.year2012.sem8.networks.timesync;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.nio.ByteBuffer;

public class NetworkListener extends Thread {
    private static final Logger log = LoggerFactory.getLogger(ConsoleListener.class);
    private final String serverName;
    private final int port;

    public NetworkListener(String serverName, int port) {
        this.serverName = serverName;
        this.port = port;
    }

    @Override
    public void run() {
        try (DatagramSocket socket = new DatagramSocket(port)) {
            log.info("Launching server...");
            while (true) {
                if (Thread.interrupted()) {
                    Thread.currentThread().interrupt();
                    break;
                }
                log.info("Receiving packet...");
                byte[] inData = new byte[69];
                DatagramPacket inPacket = new DatagramPacket(inData, inData.length);
                socket.receive(inPacket);
                String request = new String(inPacket.getData(), inPacket.getOffset(), inPacket.getLength()).trim();
                log.info("Received packet {}", request);
                if ("time".equals(request)) {
                    byte [] bytes = serverName.getBytes();
                    ByteBuffer buffer = ByteBuffer.allocate(Long.BYTES + bytes.length);
                    buffer.putLong(System.currentTimeMillis());
                    buffer.put(bytes);
                    byte[] outData = buffer.array();
                    DatagramPacket outPacket = new DatagramPacket(outData, outData.length, inPacket.getAddress(), inPacket.getPort());
                    socket.send(outPacket);
                    log.info("Packet sent");
                }
            }
        } catch (IOException e) {
            log.error("Console listener exception", e);
        }
    }
}
