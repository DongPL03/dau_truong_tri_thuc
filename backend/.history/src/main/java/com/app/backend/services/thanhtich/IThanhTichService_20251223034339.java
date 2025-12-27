package com.app.backend.services.thanhtich;

import com.app.backend.models.constant.AchievementCode;
import com.app.backend.responses.achievement.AchievementResponse;

import java.util.List;

public interface IThanhTichService {

    /**
     * Lấy toàn bộ thành tích của 1 user (dùng cho trang profile)
     */
    List<AchievementResponse> getAchievementsOfUser(Long userId);

    /**
     * Gọi sau khi cập nhật BXH (sau mỗi trận):
     * đọc BangXepHang, kiểm tra điều kiện và unlock nếu cần.
     * Trả về danh sách thành tích mới (vừa được mở).
     */
    List<AchievementResponse> processAfterBattle(Long userId);

    /**
     * Gọi sau khi hoàn thành khóa học:
     * kiểm tra điều kiện và unlock achievement nếu cần.
     * Trả về danh sách thành tích mới (vừa được mở).
     */
    List<AchievementResponse> processAfterCourseComplete(Long userId);

    /**
     * Unlock một achievement cụ thể cho user
     */
    AchievementResponse unlockAchievement(Long userId, AchievementCode achievementCode);
}
