package com.app.backend.services.khoahoc;

import com.app.backend.models.TienDoKhoaHoc;
import com.app.backend.models.TienDoBoCauHoiTrongKhoa;

public interface ITienDoKhoaHoiService {
    
    /**
     * Khởi tạo hoặc lấy progress của user trong một khóa học
     */
    TienDoKhoaHoc getOrCreateProgress(Long userId, Long khoaHocId);
    
    /**
     * Cập nhật progress sau khi user submit một practice session
     * @param userId User ID
     * @param boCauHoiId Bộ câu hỏi ID
     * @param diemSo Điểm số đạt được
     * @param doChinhXac Độ chính xác (%)
     */
    void updateProgressAfterPractice(Long userId, Long boCauHoiId, Integer diemSo, java.math.BigDecimal doChinhXac);
    
    /**
     * Lấy progress chi tiết của một bộ câu hỏi trong khóa học
     */
    TienDoBoCauHoiTrongKhoa getOrCreateBoCauHoiProgress(Long tienDoKhoaHocId, Long boCauHoiId);
    
    /**
     * Kiểm tra và tự động unlock bộ câu hỏi tiếp theo nếu đã hoàn thành bộ hiện tại
     */
    void checkAndUnlockNextBoCauHoi(Long userId, Long khoaHocId, Long completedBoCauHoiId);
}

