package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Duration;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
public class BattleState {
    /**
     * ID trận đấu
     */
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    /**
     * Danh sách câu hỏi đã được random khi start
     */
    @Singular("q")
    @JsonProperty("danh_sach_cau_hoi")
    private List<CauHoi> danhSachCauHoi;

    /**
     * Index câu hiện tại (0-based). -1 = chưa bật câu nào
     */
    @Builder.Default
    @JsonProperty("cau_hien_tai_index")
    private int currentQuestionIndex = -1;

    /**
     * Mốc giờ bắt đầu câu hiện tại (server time)
     */
    @JsonProperty("cau_hien_tai_bat_dau")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant currentQuestionStart;

    /**
     * Thời điểm bắt đầu & kết thúc trận (tham chiếu)
     */
    @JsonProperty("bat_dau_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant startTime;

    @JsonProperty("ket_thuc_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant endTime;

    /**
     * Số giây cho mỗi câu – dùng cho sync & tính hết giờ
     */
    @Builder.Default
    @JsonProperty("seconds_per_question")
    private int secondsPerQuestion = 15;

    /**
     * userId -> tổng điểm hiện tại
     */
    @Builder.Default
    @JsonProperty("diem_nguoi_choi")
    private Map<Long, Integer> diemNguoiChoi = new ConcurrentHashMap<>();

    /**
     * userId -> chuỗi đúng liên tiếp (combo)
     */
    @Builder.Default
    @JsonProperty("combo_streaks")
    private Map<Long, Integer> comboStreaks = new ConcurrentHashMap<>();


    /**
     * userId -> chuỗi đúng dài nhất trong cả trận (max combo)
     */
    @Builder.Default
    @JsonProperty("max_combo_streaks")
    private Map<Long, Integer> maxComboStreaks = new ConcurrentHashMap<>();

    /**
     * answers: questionIndex -> (userId -> answer)
     * Dùng ConcurrentHashMap lồng nhau + putIfAbsent để chống nộp trùng.
     */
    @Builder.Default
    @JsonProperty("answers")
    private Map<Integer, ConcurrentHashMap<Long, String>> answers = new ConcurrentHashMap<>();

    /**
     * userId -> index câu hỏi cuối cùng mà user này ĐÃ trả lời
     * Dùng để kiểm tra xem combo có bị "đứt" vì bỏ qua câu hay không.
     */
    @Builder.Default
    @JsonProperty("last_answered_index")
    private Map<Long, Integer> lastAnsweredQuestionIndex = new ConcurrentHashMap<>();

    /**
     * cờ cho auto loop
     */
    @Builder.Default
    @JsonProperty("auto_loop_running")
    private boolean autoLoopRunning = false;

    @Builder.Default
    @JsonProperty("marked_finished")
    private boolean markedFinished = false;

    // ================== POWER-UPS / ITEMS STATE ==================

    /**
     * userId -> hệ số nhân điểm đang active (x2, x3...)
     * Được reset sau khi trả lời câu hỏi
     */
    @Builder.Default
    @JsonProperty("active_multipliers")
    private Map<Long, Double> activeMultipliers = new ConcurrentHashMap<>();

    /**
     * userId -> số giây thêm cho câu hiện tại (đóng băng thời gian)
     */
    @Builder.Default
    @JsonProperty("extra_time_seconds")
    private Map<Long, Integer> extraTimeSeconds = new ConcurrentHashMap<>();

    /**
     * userId -> set các đáp án đã bị loại (50-50)
     */
    @Builder.Default
    @JsonProperty("eliminated_options")
    private Map<Long, Set<String>> eliminatedOptions = new ConcurrentHashMap<>();

    /**
     * Danh sách user đang có khiên bảo vệ combo
     */
    @Builder.Default
    @JsonProperty("shielded_players")
    private Set<Long> shieldedPlayers = ConcurrentHashMap.newKeySet();

    /**
     * userId -> set các câu hỏi đã bỏ qua (dùng item skip)
     */
    @Builder.Default
    @JsonProperty("skipped_questions")
    private Map<Long, Set<Integer>> skippedQuestions = new ConcurrentHashMap<>();

    /**
     * userId -> danh sách vật phẩm đã dùng trong trận (để giới hạn)
     */
    @Builder.Default
    @JsonProperty("used_items")
    private Map<Long, List<String>> usedItems = new ConcurrentHashMap<>();


    // ✅ Thêm: helper để thêm câu trả lời
    public boolean addAnswer(Long userId, int questionIdx, String answer) {
        answers.putIfAbsent(questionIdx, new ConcurrentHashMap<>());
        var map = answers.get(questionIdx);
        if (map.containsKey(userId)) return false; // đã trả lời
        map.put(userId, answer);
        return true;
    }

    /**
     * Cộng điểm cho người chơi (thread-safe)
     *
     * @return tổng điểm mới của người chơi
     */
    public int addScore(Long userId, int gained) {
        AtomicInteger newScore = new AtomicInteger();
        diemNguoiChoi.merge(userId, gained, (oldVal, add) -> {
            int total = (oldVal != null ? oldVal : 0) + add;
            newScore.set(total);
            return total;
        });
        return newScore.get();
    }

    /**
     * Cập nhật combo cho 1 người chơi khi HỌ TRẢ LỜI MỘT CÂU HỎI.
     *
     * @param userId        id người chơi
     * @param questionIndex index câu hiện tại (0-based)
     * @param correct       trả lời đúng hay không
     * @return combo mới sau khi cập nhật
     */
    public int updateCombo(Long userId, int questionIndex, boolean correct) {
        Integer lastIdx = lastAnsweredQuestionIndex.get(userId);
        int currentStreak = comboStreaks.getOrDefault(userId, 0);

        boolean continuous = (lastIdx != null && lastIdx == (questionIndex - 1));

        if (!correct) {
            // Sai → reset combo
            currentStreak = 0;
        } else {
            // Đúng mà KHÔNG liên tiếp ngay sau câu trước → combo mới = 1
            if (!continuous) {
                currentStreak = 1;
            } else {
                currentStreak = currentStreak + 1;
            }
        }

        lastAnsweredQuestionIndex.put(userId, questionIndex);
        comboStreaks.put(userId, currentStreak);
        int oldMax = maxComboStreaks.getOrDefault(userId, 0);
        if (currentStreak > oldMax) {
            maxComboStreaks.put(userId, currentStreak);
        }

        return currentStreak;
    }

    /**
     * Lấy chuỗi đúng liên tiếp (combo) hiện tại của người chơi
     */
    public int getComboStreak(Long userId) {
        return comboStreaks.getOrDefault(userId, 0);
    }


    /**
     * Lấy chuỗi đúng dài nhất trong cả trận (max combo) của người chơi
     */
    public int getMaxComboStreak(Long userId) {
        return maxComboStreaks.getOrDefault(userId, 0);
    }



    /* ===================== Helper / Business APIs ===================== */

    /**
     * Lấy câu hỏi hiện tại (có thể null nếu chưa bật câu nào)
     */
    public CauHoi getCurrentQuestion() {
        if (danhSachCauHoi == null) return null;
        if (currentQuestionIndex < 0 || currentQuestionIndex >= danhSachCauHoi.size()) return null;
        return danhSachCauHoi.get(currentQuestionIndex);
    }

    /**
     * Khởi tạo điểm = 0 cho toàn bộ người chơi ngay khi start
     */
    public void initScoresForPlayers(Set<Long> userIds) {
        if (userIds == null) return;
        for (Long uid : userIds) {
            diemNguoiChoi.putIfAbsent(uid, 0);
        }
    }


    /**
     * Kiểm tra đã trả lời câu idx chưa
     */
    public boolean hasAnswered(int questionIndex, Long userId) {
        var bucket = answers.get(questionIndex);
        return bucket != null && bucket.containsKey(userId);
    }

    /**
     * Ghi đáp án cho câu idx của user (atomic).
     *
     * @return true nếu ghi thành công; false nếu đã có đáp án trước đó.
     */
    public synchronized boolean recordAnswer(int questionIndex, Long userId, String answer) {
        Map<Long, String> map = answers.computeIfAbsent(questionIndex, k -> new ConcurrentHashMap<>());
        if (map.containsKey(userId)) {
            return false; // đã nộp
        }
        map.put(userId, answer);
        return true;
    }

    /**
     * Số người đã trả lời câu idx (tiện cho thống kê)
     */
    public int answeredCount(int questionIndex) {
        var bucket = answers.get(questionIndex);
        return bucket == null ? 0 : bucket.size();
    }

    /**
     * Trận đã kết thúc chưa?
     * - Kết thúc nếu endTime đã được set (service finishBattle đặt).
     * - Hoặc đã ở câu cuối cùng và đã quá thời gian của câu cuối.
     */
    public boolean isFinished() {
        if (markedFinished || endTime != null) return true;
        if (danhSachCauHoi == null || danhSachCauHoi.isEmpty()) return false;

        boolean atLast = currentQuestionIndex >= (danhSachCauHoi.size() - 1);
        if (!atLast || currentQuestionStart == null) return false;

        long elapsedMs = Duration.between(currentQuestionStart, Instant.now()).toMillis();
        return elapsedMs >= (long) secondsPerQuestion * 1000L;
    }


    public synchronized boolean markFinishedOnce() {
        if (markedFinished) return false;
        markedFinished = true;
        endTime = Instant.now();
        return true;
    }

    public boolean isMarkedFinished() {
        return markedFinished;
    }

    public int getTotalPlayers() {
        return diemNguoiChoi.size();
    }
}

/**/