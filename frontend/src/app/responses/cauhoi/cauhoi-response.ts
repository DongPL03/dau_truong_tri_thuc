export interface CauHoiResponse {
  id: number;
  noi_dung: string;
  do_kho: 'DE' | 'TRUNG_BINH' | 'KHO';
  loai_noi_dung: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
  duong_dan_tep?: string | null;

  lua_chon_a: string;
  lua_chon_b: string;
  lua_chon_c: string;
  lua_chon_d: string;
  dap_an_dung: 'A' | 'B' | 'C' | 'D';
  giai_thich?: string;

  bo_cau_hoi_id: number;
  bo_cau_hoi_tieu_de?: string;
}
