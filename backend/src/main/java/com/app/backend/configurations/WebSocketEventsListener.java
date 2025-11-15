package com.app.backend.configurations;

import com.app.backend.components.BattleWsPublisher;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.INguoiChoiTranDauRepository;
import com.app.backend.repositories.INguoiDungRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.messaging.SessionConnectEvent;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Lắng nghe sự kiện connect/disconnect từ STOMP WebSocket.
 * Dùng để phát event join/leave và giữ mapping sessionId → userId/tranDauId.
 */
@Component
@RequiredArgsConstructor
public class WebSocketEventsListener {

    private final INguoiDungRepository nguoiDungRepo;
    private final INguoiChoiTranDauRepository nctdRepo;
    private final BattleWsPublisher wsPublisher;

    // sessionId → userId / tranDauId
    private final Map<String, Long> sessionToUser = new ConcurrentHashMap<>();
    private final Map<String, Long> sessionToTranDau = new ConcurrentHashMap<>();

    @EventListener
    public void onConnect(SessionConnectEvent event) {
        var accessor = StompHeaderAccessor.wrap(event.getMessage());
        String sessionId = accessor.getSessionId();

        System.out.println("📡 New WS CONNECT from sessionId=" + sessionId);
        System.out.println("📡 Headers = " + accessor.toNativeHeaderMap());

        String uidStr = accessor.getFirstNativeHeader("x-user-id");
        String tdStr = accessor.getFirstNativeHeader("x-trandau-id");

        try {
            Long userId = Long.parseLong(uidStr);
            Long tranDauId = Long.parseLong(tdStr);

            sessionToUser.put(sessionId, userId);
            sessionToTranDau.put(sessionId, tranDauId);

            Optional<NguoiDung> userOpt = nguoiDungRepo.findById(userId);
            if (userOpt.isEmpty()) return;

            int soNguoi = (int) nctdRepo.countByTranDau_Id(tranDauId);

            wsPublisher.publishPlayerJoined(tranDauId, userId, userOpt.get().getHoTen(), soNguoi);

            System.out.printf("✅ [%s] User #%d joined room #%d (%d người hiện tại)%n",
                    sessionId, userId, tranDauId, soNguoi);
        } catch (Exception e) {
            System.err.printf("❌ Lỗi onConnect (sessionId=%s): %s%n", sessionId, e.getMessage());
        }
    }


    @EventListener
    public void onDisconnect(SessionDisconnectEvent event) {
        String sessionId = StompHeaderAccessor.wrap(event.getMessage()).getSessionId();
        Long userId = sessionToUser.remove(sessionId);
        Long tranDauId = sessionToTranDau.remove(sessionId);

        if (userId == null || tranDauId == null) {
            System.out.printf("⚠️ Bỏ qua disconnect không có mapping (sessionId=%s)%n", sessionId);
            return;
        }

        try {
            Optional<NguoiDung> userOpt = nguoiDungRepo.findById(userId);
            if (userOpt.isEmpty()) return;

            // Đếm số người hiện tại còn trong phòng
            int soNguoi = (int) nctdRepo.countByTranDau_Id(tranDauId);

            // Phát event "PLAYER_LEFT"
            wsPublisher.publishPlayerLeft(
                    tranDauId,
                    userId,
                    userOpt.get().getHoTen(),
                    soNguoi
            );

            System.out.printf("👋 [%s] User #%d left room #%d (%d người còn lại)%n",
                    sessionId, userId, tranDauId, soNguoi);

        } catch (Exception e) {
            System.err.printf("❌ Lỗi onDisconnect: %s%n", e.getMessage());
        }
    }
}
