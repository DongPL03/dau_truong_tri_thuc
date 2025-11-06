package com.app.backend.components;

import com.app.backend.models.CauHoi;
import com.app.backend.responses.websocket.FinishedEvent;
import com.app.backend.responses.websocket.NewQuestionEvent;
import com.app.backend.responses.websocket.ScoreUpdateEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class BattleWsPublisher {

    private final SimpMessagingTemplate messagingTemplate;

    private String topic(Long tranDauId) {
        return "/topic/battle." + tranDauId;
    }

    public void publishNewQuestion(Long tranDauId, int idx, CauHoi q, int seconds) {
        var payload = NewQuestionEvent.builder()
                .type("NEW_QUESTION")
                .tranDauId(tranDauId)
                .questionIndex(idx)
                .thoiGianCauGiay(seconds)
                .question(NewQuestionEvent.QuestionView.builder()
                        .id(q.getId())
                        .noiDung(q.getNoiDung())
                        .luaChonA(q.getLuaChonA())
                        .luaChonB(q.getLuaChonB())
                        .luaChonC(q.getLuaChonC())
                        .luaChonD(q.getLuaChonD())
                        .build())
                .build();
        messagingTemplate.convertAndSend(topic(tranDauId), payload);
    }

    public void publishScoreUpdate(Long tranDauId, Long userId, String hoTen,
                                   boolean correct, int gained, int total, int questionIndex) {
        var payload = ScoreUpdateEvent.builder()
                .type("SCORE_UPDATE")
                .tranDauId(tranDauId)
                .userId(userId)
                .hoTen(hoTen)
                .correct(correct)
                .gainedPoints(gained)
                .totalPoints(total)
                .questionIndex(questionIndex)
                .build();
        messagingTemplate.convertAndSend(topic(tranDauId), payload);
    }

    public void publishFinished(Long tranDauId, String tenPhong, String maPhong,
                                java.time.LocalDateTime batDau, java.time.LocalDateTime ketThuc,
                                FinishedEvent.Winner winner,
                                List<FinishedEvent.Player> leaderboard) {
        var payload = FinishedEvent.builder()
                .type("FINISHED")
                .tranDauId(tranDauId)
                .tenPhong(tenPhong)
                .maPhong(maPhong)
                .batDauLuc(batDau)
                .ketThucLuc(ketThuc)
                .winner(winner)
                .leaderboard(leaderboard)
                .build();
        messagingTemplate.convertAndSend(topic(tranDauId), payload);
    }
}
