// ================== QUEST MODELS ==================

export type LoaiNhiemVu = 'DAILY' | 'WEEKLY';

export interface QuestItem {
  ma: string;
  mo_ta: string;
  icon: string;
  loai: LoaiNhiemVu;
  tien_do: number;
  muc_tieu: number;
  phan_tram: number;
  da_hoan_thanh: boolean;
  da_nhan_thuong: boolean;
  gold_thuong: number;
  xp_thuong: number;
  vat_pham_loai: string | null;
}

export interface NhiemVuResponse {
  daily_quests: QuestItem[];
  weekly_quests: QuestItem[];
  daily_completed: number;
  daily_total: number;
  weekly_completed: number;
  weekly_total: number;
}

export interface QuestRewardItem {
  loai: 'GOLD' | 'XP' | 'VAT_PHAM';
  ten: string;
  so_luong: number;
  icon: string;
}

export interface NhanThuongNhiemVuResponse {
  thanh_cong: boolean;
  thong_bao: string;
  phan_thuong: QuestRewardItem[];
  gold_moi: number;
}
