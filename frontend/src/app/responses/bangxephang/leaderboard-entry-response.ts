export interface LeaderboardEntryResponse {
  user_id: number;
  ho_ten: string;
  anh_dai_dien?: string | null;

  tong_diem: number;
  tong_tran: number;
  so_tran_thang: number;
  so_tran_thua: number;

  ti_le_thang: number; // % thắng, backend trả double
  xep_hang: number;
  level: number;
  rank_tier: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
}
