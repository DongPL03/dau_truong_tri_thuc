package com.app.backend.components;

import com.app.backend.models.CauHoi;
import com.app.backend.responses.websocket.*;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;

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
                                     LocalDateTime batDau,
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
                .timestamp(LocalDateTime.now())
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
                                LocalDateTime batDau, LocalDateTime ketThuc,
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
                .timestamp(LocalDateTime.now())
                .winner(winner)
                .leaderboard(leaderboard)
                .build();
        safeSend(tranDauId, payload);
    }

    /* ==================== SCORE & LEADERBOARD ==================== */

    public void publishScoreUpdate(Long tranDauId, Long userId, String hoTen,
                                   boolean correct, int gained, int total, int questionIndex) {
        var payload = ScoreUpdateEvent.builder()
                .type("SCORE_UPDATE")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .correct(correct)
                .timestamp(LocalDateTime.now())
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(questionIndex)
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

    private void safeSendToUser(Long userId, Object payload) {
        try {
            messagingTemplate.convertAndSendToUser(
                    String.valueOf(userId), // username của user session
                    "/queue/battle",        // đích riêng user sẽ subscribe
                    payload
            );
        } catch (Exception e) {
            System.err.println("⚠️ [WS ERROR] Không thể gửi WS cá nhân: " + e.getMessage());
        }
    }
}
