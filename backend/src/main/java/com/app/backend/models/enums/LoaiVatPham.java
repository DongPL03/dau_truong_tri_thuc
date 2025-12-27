package com.app.backend.models.enums;

/**
 * Enum các loại vật phẩm (Power-ups) trong trận đấu
 */
public enum LoaiVatPham {
    /**
     * Nhân đôi điểm cho câu tiếp theo
     */
    X2_DIEM,

    /**
     * Dừng đồng hồ thêm 5 giây
     */
    DONG_BANG_THOI_GIAN,

    /**
     * Bỏ qua câu hỏi hiện tại mà không bị mất điểm hay đứt combo
     */
    BO_QUA_CAU_HOI,

    /**
     * Gợi ý 50/50: Loại bỏ 2 đáp án sai
     */
    GOI_Y_50_50,

    /**
     * Khiên bảo vệ: Không mất combo khi trả lời sai 1 lần
     */
    KHIEN_BAO_VE,

    /**
     * Nhân ba điểm cho câu tiếp theo (hiếm)
     */
    X3_DIEM,

    /**
     * Hiển thị đáp án đúng (rất hiếm - chỉ dùng 1 lần/trận)
     */
    HIEN_DAP_AN
}
