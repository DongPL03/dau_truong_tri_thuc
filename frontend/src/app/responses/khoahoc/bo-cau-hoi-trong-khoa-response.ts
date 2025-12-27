export interface BoCauHoiTrongKhoaResponse {
  id: number;
  bo_cau_hoi_id: number;
  tieu_de: string;
  so_cau_hoi: number;
  thu_tu: number;
  is_bat_buoc: boolean;
  diem_toi_thieu: number;
  trang_thai: string; // CHUA_MO_KHOA, DA_MO_KHOA, DANG_HOC, HOAN_THANH
  da_mo_khoa: boolean;
  can_mo_khoa: boolean;
  gia_mo_khoa: number;
  so_cau_sai?: number; // Số lượng câu hỏi đã trả lời sai (từ the_ghi_nho)
}
