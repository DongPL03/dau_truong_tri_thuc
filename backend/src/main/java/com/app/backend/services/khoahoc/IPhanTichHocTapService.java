package com.app.backend.services.khoahoc;

import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.responses.khoahoc.PhanTichHocTapResponse;

public interface IPhanTichHocTapService {
    /**
     * Phân tích mạnh yếu cho một khóa học của user
     * @param userId ID của user
     * @param khoaHocId ID của khóa học
     * @return PhanTichHocTapResponse chứa kết quả phân tích
     * @throws DataNotFoundException nếu khóa học không tồn tại
     */
    PhanTichHocTapResponse phanTichKhoaHoc(Long userId, Long khoaHocId) throws DataNotFoundException;

    /**
     * Lấy kết quả phân tích đã lưu (nếu có)
     * @param userId ID của user
     * @param khoaHocId ID của khóa học
     * @return PhanTichHocTapResponse hoặc null nếu chưa có phân tích
     */
    PhanTichHocTapResponse getPhanTich(Long userId, Long khoaHocId);
}

