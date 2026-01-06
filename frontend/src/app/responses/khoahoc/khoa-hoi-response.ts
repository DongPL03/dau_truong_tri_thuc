export interface KhoaHoiResponse {
  id: number;
  tieu_de: string;
  mo_ta?: string;
  hinh_anh?: string;
  chu_de?: string;
  chu_de_id?: number;
  nguoi_tao?: string;
  nguoi_tao_id?: number;
  trang_thai?: string; // DRAFT, PUBLISHED, ARCHIVED
  gia_mo_khoa?: number;
  thu_tu?: number;
  so_bo_cau_hoi?: number;
  co_quyen_sua?: boolean;
  tao_luc?: string;
  cap_nhat_luc?: string;
  // Rating fields
  tong_danh_gia?: number;
  trung_binh_sao?: number;
}
