export interface LichSuLuyenTapUserInfo {
  id: number;
  ho_ten: string;
}

export interface LichSuLuyenTapItem {
  phien_id: number;
  bo_cau_hoi: string;
  tong_cau_hoi: number;
  so_cau_dung: number;
  diem_so: number;
  do_chinh_xac: number;
  thoi_gian_tb_ms: number;
  ngay_tao: string;
  nguoi_dung: LichSuLuyenTapUserInfo;
}
