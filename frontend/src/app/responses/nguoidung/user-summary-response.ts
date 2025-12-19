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
  tong_xp: number;
  level: number;
  rank_tier: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';

  xp_in_current_level: number;
  xp_next_level: number;
  level_progress_percent: number;

  tien_vang: number;
  xep_hang: number;
  lich_su_tran_dau?: LichSuTranDauResponse[];
}
