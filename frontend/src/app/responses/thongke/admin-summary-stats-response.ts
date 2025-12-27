// src/app/responses/thongke/admin-summary-stats-response.ts

export interface AdminSummaryStatsResponse {
  // Người dùng
  tong_nguoi_dung: number;
  nguoi_dung_active: number;
  nguoi_dung_blocked: number;
  nguoi_dung_deleted: number;
  so_admin: number;
  nguoi_dung_moi_hom_nay: number;

  // Trận đấu
  tong_tran_dau: number;
  tran_dang_cho: number;
  tran_dang_dien_ra: number;
  tran_da_ket_thuc: number;
  tran_da_huy: number;
  tran_hom_nay: number;

  // Bộ câu hỏi
  tong_bo_cau_hoi: number;
  bo_cau_hoi_da_duyet: number;
  bo_cau_hoi_cho_duyet: number;
  bo_cau_hoi_tu_choi: number;
  tong_cau_hoi: number;

  // Khóa học
  tong_khoa_hoc: number;
  khoa_hoc_published: number;
  khoa_hoc_draft: number;
}
