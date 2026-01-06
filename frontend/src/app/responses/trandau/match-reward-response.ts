export interface LevelUpRewardItem {
  loai: 'GOLD' | 'VAT_PHAM' | 'XP';
  ten: string;
  so_luong: number;
  icon: string;
  cap_do: number;
}

export interface MatchRewardResponse {
  xp_gained: number;
  gold_gained: number;
  level_before: number;
  level_after: number;
  rank_tier_before: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
  rank_tier_after: 'BRONZE' | 'SILVER' | 'GOLD' | 'PLATINUM' | 'DIAMOND' | 'MASTER';
  leveled_up?: boolean;
  level_up_rewards?: LevelUpRewardItem[];
}
