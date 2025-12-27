package com.app.backend.components;

import com.app.backend.models.CauHoi;
import com.app.backend.responses.websocket.*;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Component phụ trách broadcast toàn bộ sự kiện trận đấu qua WebSocket.
 * Tất cả client đang subscribe /topic/battle.{tranDauId} sẽ nhận thông báo realtime.
 */
@Component
@RequiredArgsConstructor
public class BattleWsPublisher {

    private final SimpMessagingTemplate messagingTemplate;

    /**
     * Tạo topic WS theo ID trận
     */
    private String topic(Long tranDauId) {
        return "/topic/battle." + tranDauId;
    }

    /**
     * Gửi WS an toàn — tránh crash nếu client chưa subscribe
     */
    private void safeSend(Long tranDauId, Object payload) {
        try {
            messagingTemplate.convertAndSend(topic(tranDauId), payload);
        } catch (Exception e) {
            System.err.println("⚠️ [WS ERROR] Không thể gửi sự kiện WS: " + e.getMessage());
        }
    }

    /* ==================== PLAYER EVENTS ==================== */

    public void publishPlayerJoined(Long tranDauId, Long userId, String hoTen, int soNguoiHienTai) {
        var payload = PlayerPresenceEvent.builder()
                .type("PLAYER_JOINED")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .soNguoiHienTai(soNguoiHienTai)
                .build();
        safeSend(tranDauId, payload);
    }

    public void publishPlayerLeft(Long tranDauId, Long userId, String hoTen, int soNguoiHienTai) {
        var payload = PlayerPresenceEvent.builder()
                .type("PLAYER_LEFT")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .soNguoiHienTai(soNguoiHienTai)
                .build();
        safeSend(tranDauId, payload);
    }

    /* ==================== BATTLE LIFECYCLE ==================== */

    public void publishBattleStarted(Long tranDauId,
                                     String tenPhong,
                                     Instant batDau,
                                     int tongCauHoi,
                                     int secondsPerQuestion,
                                     int preCountdownSeconds) {
        var payload = BattleStartedEvent.builder()
                .type("BATTLE_STARTED")
                .tranDauId(tranDauId)
                .tenPhong(tenPhong)
                .batDauLuc(batDau)
                .tongCauHoi(tongCauHoi)
                .thoiGianMoiCauGiay(secondsPerQuestion)
                .demNguocTruocGiay(preCountdownSeconds)
                .build();
        safeSend(tranDauId, payload);
    }

    public void publishNewQuestion(Long tranDauId, int idx, CauHoi q, int secondsPerQuestion) {
        if (q == null) return;
        var payload = NewQuestionEvent.builder()
                .type("NEW_QUESTION")
                .tranDauId(tranDauId)
                .questionIndex(idx)
                .timestamp(Instant.now())
                .thoiGianCauGiay(secondsPerQuestion)
                .question(NewQuestionEvent.QuestionView.builder()
                        .id(q.getId())
                        .noiDung(q.getNoiDung())
                        .loaiNoiDung(q.getLoaiNoiDung())
                        .duongDanTep(q.getDuongDanTep())
                        .luaChonA(q.getLuaChonA())
                        .luaChonB(q.getLuaChonB())
                        .luaChonC(q.getLuaChonC())
                        .luaChonD(q.getLuaChonD())
                        .build())
                .build();
        safeSend(tranDauId, payload);
    }

    public void publishFinished(Long tranDauId, String tenPhong, String maPhong,
                                Instant batDau, Instant ketThuc,
                                FinishedEvent.Winner winner,
                                List<FinishedEvent.Player> leaderboard) {

        System.out.println("📤 [WS] FINISHED gửi tới /topic/battle." + tranDauId
                + " | leaderboard size = " + (leaderboard != null ? leaderboard.size() : 0));
        var payload = FinishedEvent.builder()
                .type("FINISHED")
                .tranDauId(tranDauId)
                .tenPhong(tenPhong)
                .maPhong(maPhong)
                .batDauLuc(batDau)
                .ketThucLuc(ketThuc)
                .timestamp(Instant.now())
                .winner(winner)
                .leaderboard(leaderboard)
                .build();
        safeSend(tranDauId, payload);
    }

    /* ==================== SCORE & LEADERBOARD ==================== */

    public void publishScoreUpdate(Long tranDauId, Long userId, String hoTen,
                                   boolean correct, int gained, int total, int questionIndex,
                                   int comboStreak, int comboBonus, double comboMultiplier) {
        var payload = ScoreUpdateEvent.builder()
                .type("SCORE_UPDATE")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .correct(correct)
                .timestamp(Instant.now())
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(questionIndex)
                .comboStreak(comboStreak)
                .comboBonus(comboBonus)
                .comboMultiplier(comboMultiplier)
                .build();
        safeSend(tranDauId, payload);
    }



    public void publishLeaderboard(Long tranDauId, List<LeaderboardUpdateEvent.Row> allPlayers) {
        var payload = LeaderboardUpdateEvent.builder()
                .type("LEADERBOARD_UPDATE")
                .tranDauId(tranDauId)
                .players(allPlayers)
                .build();
        safeSend(tranDauId, payload);
    }

    public void publishChatMessage(Long tranDauId,
                                   Long userId,
                                   String hoTen,
                                   String noiDung,
                                   boolean system) {
        var payload = ChatMessageEvent.builder()
                .type("CHAT_MESSAGE")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .noiDung(noiDung)
                .system(system)
                .timestamp(Instant.now())
                .build();
        safeSend(tranDauId, payload);
    }

    public void publishAnswerReveal(Long tranDauId,
                                    Long cauHoiId,
                                    String dapAnDung,
                                    String giaiThich) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "ANSWER_REVEAL");
        payload.put("tran_dau_id", tranDauId);
        payload.put("cau_hoi_id", cauHoiId);
        payload.put("dap_an_dung", dapAnDung);   // "A" | "B" | "C" | "D"
        payload.put("giai_thich", giaiThich);    // có thể null

        messagingTemplate.convertAndSend("/topic/battle." + tranDauId, payload);
    }

    /* ==================== ADMIN EVENTS ==================== */

    /**
     * Admin đóng phòng
     */
    public void sendRoomClosed(Long tranDauId, String reason) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "ROOM_CLOSED");
        payload.put("tran_dau_id", tranDauId);
        payload.put("reason", reason);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }

    /**
     * Admin kick người chơi
     */
    public void sendPlayerKicked(Long tranDauId, Long userId, String reason) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "PLAYER_KICKED");
        payload.put("tran_dau_id", tranDauId);
        payload.put("user_id", userId);
        payload.put("reason", reason);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }

    /* ==================== POWER-UPS / ITEMS ==================== */

    /**
     * Broadcast sự kiện chung (generic) cho power-ups, items, etc.
     */
    public void publishGeneric(Long tranDauId, String eventType, Object data) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", eventType);
        payload.put("tran_dau_id", tranDauId);
        payload.put("data", data);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }

    /**
     * Broadcast khi ai đó dùng vật phẩm
     */
    public void publishItemUsed(Long tranDauId, Long userId, String hoTen,
                                 String loaiVatPham, String tenVatPham,
                                 Object hieuUng) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "ITEM_USED");
        payload.put("tran_dau_id", tranDauId);
        payload.put("user_id", userId);
        payload.put("ho_ten", hoTen);
        payload.put("loai_vat_pham", loaiVatPham);
        payload.put("ten_vat_pham", tenVatPham);
        payload.put("hieu_ung", hieuUng);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }

    /**
     * Broadcast hiệu ứng 50/50 (các đáp án bị loại)
     */
    public void publish5050Effect(Long tranDauId, Long userId, List<String> dapAnBiLoai) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "EFFECT_50_50");
        payload.put("tran_dau_id", tranDauId);
        payload.put("user_id", userId);
        payload.put("dap_an_bi_loai", dapAnBiLoai);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }

    /**
     * Broadcast khi có người kích hoạt x2/x3 điểm
     */
    public void publishMultiplierActive(Long tranDauId, Long userId, String hoTen, double multiplier) {
        Map<String, Object> payload = new HashMap<>();
        payload.put("type", "MULTIPLIER_ACTIVE");
        payload.put("tran_dau_id", tranDauId);
        payload.put("user_id", userId);
        payload.put("ho_ten", hoTen);
        payload.put("multiplier", multiplier);
        payload.put("timestamp", Instant.now());
        safeSend(tranDauId, payload);
    }
}
