import {FinishedPlayer} from './finished-player';

export interface LichSuTranDauQuestionResponse {
  cau_hoi_id: number;
  noi_dung: string;
  loai_noi_dung: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
  duong_dan_tep?: string | null;
  lua_chon_a: string;
  lua_chon_b: string;
  lua_chon_c: string;
  lua_chon_d: string;
  dap_an_dung: 'A' | 'B' | 'C' | 'D';
  giai_thich?: string | null;
  nguoi_dung_chon: 'A' | 'B' | 'C' | 'D';
  dung_hay_sai: boolean;
  thoi_gian_ms: number;
}

export interface LichSuTranDauDetailResponse {
  tran_dau_id: number;
  ten_phong: string | null;
  ma_phong: string;
  bo_cau_hoi_tieu_de: string | null;
  luat_tinh_diem: string;
  bat_dau_luc: string | null;
  ket_thuc_luc: string | null;

  my_tong_diem: number;
  my_so_cau_dung: number;
  my_xep_hang: number;
  my_tong_thoi_gian_ms: number;

  leaderboard: FinishedPlayer[];
  questions: LichSuTranDauQuestionResponse[];
}
