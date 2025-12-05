import {LichSuTranDauResponse} from '../lichsutrandau/lich_su_tran_dau_response';

export interface UserSummaryResponse {
  user_id: number;
  ho_ten: string;
  avatar_url?: string | null;

  tong_diem: number;
  tong_tran: number;
  so_tran_thang: number;
  so_tran_thua: number;

  ti_le_thang: number; // %
  rank_tier: string;

  xep_hang: number
  lich_su_tran_dau?: LichSuTranDauResponse[];
}
