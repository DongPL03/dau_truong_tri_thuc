export interface BoCauHoiResponse {
  id: number;

  /** 🏷️ Tiêu đề bộ câu hỏi */
  tieu_de: string;

  /** 📝 Mô tả ngắn gọn */
  mo_ta: string;

  /** 👁️ Chế độ hiển thị (PUBLIC / PRIVATE) */
  che_do_hien_thi: string;

  /** ⚙️ Trạng thái duyệt (PENDING / APPROVED / REJECTED) */
  trang_thai: string;

  /** ✅ Bộ này có được đánh dấu dùng cho thi đấu (Official) hay không */
  is_official?: boolean;

  /** ❌ Lý do bị từ chối (nếu có) */
  ly_do_tu_choi?: string;

  /** ❌ Bộ câu hỏi này đã bị xóa hay chưa */
  co_quyen_sua?: boolean;

  /** 🧩 Chủ đề (chỉ là tên, không phải object) */
  chu_de?: string;

  /** ID chủ đề */
  chu_de_id?: number;

  /** 👤 Người tạo (tên) */
  nguoi_tao?: string;

  /** ID người tạo */
  nguoi_tao_id?: number;

  /** 🕒 Thời điểm tạo */
  tao_luc?: string;

  /** 🕓 Thời điểm cập nhật gần nhất */
  cap_nhat_luc?: string;
}
