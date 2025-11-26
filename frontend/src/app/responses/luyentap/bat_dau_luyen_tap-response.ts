export interface CauHoiPracticeItem {
  id: number;
  noi_dung: string;
  loai_noi_dung: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
  duong_dan_tep: string | null;
  lua_chon_a: string;
  lua_chon_b: string;
  lua_chon_c: string;
  lua_chon_d: string;
}

export interface BatDauLuyenTapResponse {
  phien_id: number;
  bo_cau_hoi: string;
  tong_cau_hoi: number;
  cau_hoi_list: CauHoiPracticeItem[];
}
