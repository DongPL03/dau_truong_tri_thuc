export interface ChatMessage {
  user_id: number;
  avatar_url?: string | null;
  ho_ten: string;
  noi_dung: string;
  is_system: boolean;
  timestamp: string;
  is_me?: boolean;
}
