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

  /** 🎯 Loại sử dụng (PRACTICE_ONLY, RANKED_ONLY, CASUAL_ONLY) */
  loai_su_dung?: string;

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

  can_mo_khoa?: boolean; // true = phải mở khoá mới luyện được

  gia_mo_khoa?: number; // giá vàng

  da_mo_khoa?: boolean; // backend set true nếu user này đã mở khoá

  /** 💰 User muốn tạo bộ câu hỏi trả phí hay không (true = trả phí, false = miễn phí) */
  muon_tao_tra_phi?: boolean;

  /** 📊 Số lượng câu hỏi trong bộ câu hỏi */
  so_cau_hoi?: number;

  // Bộ câu hỏi có thuộc một khóa học nào đó không
  thuoc_khoa_hoc?: boolean;

  // Thông tin khóa học gắn với bộ câu hỏi (nếu có)
  khoa_hoc_id?: number;
  khoa_hoc_ten?: string;

  /** 🕓 Thời điểm cập nhật gần nhất */
  cap_nhat_luc?: string;
}
