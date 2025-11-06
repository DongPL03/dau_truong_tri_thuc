package com.app.backend.models;

import lombok.*;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class BattleState {
    private Long tranDauId;

    /**
     * Danh sách câu hỏi đã random khi start
     */
    private List<CauHoi> danhSachCauHoi;

    /**
     * Index câu hiện tại (0-based). -1 nghĩa là chưa bật câu nào
     */
    private int currentQuestionIndex = -1;

    /**
     * Mốc giờ bắt đầu câu hiện tại (server-time)
     */
    private LocalDateTime currentQuestionStart;

    /**
     * Thời điểm bắt đầu & kết thúc trận (tham chiếu)
     */
    private LocalDateTime startTime;
    private LocalDateTime endTime;

    /**
     * userId -> tổng điểm hiện tại
     */
    private Map<Long, Integer> diemNguoiChoi = new ConcurrentHashMap<>();

    /**
     * questionIndex -> (userId -> answer) để chống nộp nhiều lần
     */
    private Map<Integer, Map<Long, String>> answers = new ConcurrentHashMap<>();

    private boolean autoLoopRunning = false;

    public boolean isFinished() {
        if (currentQuestionIndex < (danhSachCauHoi.size() - 1)) return false;
        answers.getOrDefault(currentQuestionIndex, Collections.emptyMap());
        return true; // placeholder
    }
}

