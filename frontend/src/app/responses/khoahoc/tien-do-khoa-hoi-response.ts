export interface TienDoKhoaHoiResponse {
  id: number;
  khoa_hoc_id: number;
  so_bo_da_hoan_thanh: number;
  tong_so_bo: number;
  phan_tram_hoan_thanh: number;
  bo_cau_hoi_hien_tai_id?: number;
  trang_thai?: string; // CHUA_BAT_DAU, DANG_HOC, HOAN_THANH, TAM_DUNG
  ngay_bat_dau?: string;
  ngay_hoan_thanh?: string;
}
