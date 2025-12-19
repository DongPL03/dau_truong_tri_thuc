import {AchievementResponse} from '../thanhtich/achievement-response';

export interface FinishedPlayer {
  user_id: number;
  ho_ten: string;
  diem: number;
  so_cau_dung: number;
  xep_hang: number;
  max_combo?: number;

  xp_gained?: number;
  gold_gained?: number;
  level_before?: number;
  level_after?: number;
  rank_tier_before?: string; // 'BRONZE' | 'SILVER' | ...
  rank_tier_after?: string;

  new_achievements?: AchievementResponse[];
}
