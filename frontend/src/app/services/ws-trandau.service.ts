// ============================================================
// file: src/app/services/wsbattle.service.ts
// WebSocket Battle service — kết nối STOMP/SockJS tới backend
// ============================================================

import {Injectable, NgZone} from '@angular/core';
import {Client, IMessage, StompSubscription} from '@stomp/stompjs';
import SockJS from 'sockjs-client';

// ---- Kiểu dữ liệu sự kiện trận đấu ----
export type BattleEvent =
  | {
  type: 'PLAYER_JOINED' | 'PLAYER_LEFT';
  tran_dau_id: number;
  user_id: number; // Sửa từ userId
  ho_ten: string; // Sửa từ hoTen
  so_nguoi_hien_tai: number; // Sửa từ soNguoiHienTai
}
  | {
  type: 'BATTLE_STARTED';
  tran_dau_id: number; // Sửa từ tranDauId
  ten_phong: string; // Sửa từ tenPhong
  bat_dau_luc: string; // Sửa từ batDauLuc
  tong_cau_hoi: number; // Sửa từ tongCauHoi
  thoi_gian_moi_cau_giay: number; // Sửa từ thoiGianMoiCauGiay
  dem_nguoc_truoc_cau: number;
}
  | {
  type: 'NEW_QUESTION';
  tran_dau_id: number;
  question_index: number;
  thoi_gian_cau_giay: number;
  timestamp: string;
  question: QuestionPayload;
}

  | {
  type: 'SCORE_UPDATE';
  tran_dau_id: number; // Sửa từ tranDauId
  user_id: number; // Sửa từ userId
  ho_ten: string; // Sửa từ hoTen
  correct: boolean;
  gained_points: number; // Sửa từ gainedPoints
  total_points: number; // Sửa từ totalPoints
  question_index: number; // Sửa từ questionIndex
  timestamp: string;
}
  | {
  type: "LEADERBOARD_UPDATE";
  tran_dau_id: number;
  players: Array<{
    user_id: number;
    ho_ten: string;
    diem: number;
    so_cau_dung: number;
    xep_hang: number;
  }>;
}
  | {
  type: 'FINISHED';
  tran_dau_id: number; // Sửa từ tranDauId
  ten_phong: string; // Sửa từ tenPhong
  ma_phong: string; // Sửa từ maPhong
  bat_dau_luc: string; // Sửa từ batDauLuc
  ket_thuc_luc: string; // Sửa từ ketThucLuc
  timestamp: string;
  winner?: {
    user_id: number;
    ho_ten: string;
    diem: number;
    so_cau_dung: number;
  } | null;
  leaderboard: Array<{
    user_id: number;
    ho_ten: string;
    diem: number;
    so_cau_dung: number;
    xep_hang: number;
  }>;
}
  | {
  type: 'CHAT_MESSAGE';
  tran_dau_id: number;
  user_id: number;
  ho_ten: string;
  noi_dung: string;
  is_system: boolean;
  timestamp: string;
};

export interface QuestionPayload {
  id: number;
  noi_dung: string;
  loai_noi_dung: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
  duong_dan_tep?: string;
  lua_chon_a: string;
  lua_chon_b: string;
  lua_chon_c: string;
  lua_chon_d: string;
}

@Injectable({providedIn: 'root'})
export class WsTrandauService {
  private client?: Client;
  private connected = false;
  private reconnectTimeout?: any;
  private subs: Record<string, StompSubscription> = {};

  constructor(private zone: NgZone) {
  }

  /**
   * ✅ Kết nối tới backend WebSocket (STOMP)
   * - Dùng SockJS (đã fallback cho browser không hỗ trợ WS thuần)
   * - Gửi kèm header: x-user-id, x-trandau-id
   */
  async connect(getToken: () => string | null, userId: number, tranDauId: number): Promise<void> {
    // Nếu đã có client đang hoạt động thì bỏ qua
    if (this.client && this.client.active) return Promise.resolve();

    return new Promise((resolve, reject) => {
      const token = getToken();

      // ⚙️ URL backend thật — phải là port backend (8088)
      const backendUrl = 'http://localhost:8088/ws';
      const client = new Client({
        webSocketFactory: () => new SockJS(backendUrl),
        connectHeaders: {
          'x-user-id': String(userId),
          'x-trandau-id': String(tranDauId),
          Authorization: token ? `Bearer ${token}` : '',
        },
        reconnectDelay: 3000,
        heartbeatIncoming: 10000,
        heartbeatOutgoing: 10000,
        // debug: (msg) => console.log('[STOMP]', msg),
        onConnect: () => {
          // console.log('✅ STOMP connected to backend!');
          this.zone.run(() => resolve());
        },
        onStompError: (frame) => {
          // console.error('❌ STOMP error:', frame.headers['message']);
          this.zone.run(() => reject(frame.headers['message']));
        },
      });

      // console.log('🚀 Activating WS client...');
      client.activate();
      this.client = client;
    });
  }

  /**
   * Retry khi WS bị mất kết nối
   */
  private retry(getToken: () => string, userId: number, tranDauId: number) {
    if (this.reconnectTimeout) return;
    console.log('♻️ Tự động reconnect WS sau 5s...');
    this.reconnectTimeout = setTimeout(() => {
      this.connect(getToken, userId, tranDauId).then(r => {
        console.log('✅ Reconnected WS thành công!');
        this.reconnectTimeout = undefined;
      }).catch(err => {
        console.error('❌ Reconnect WS thất bại:', err);
        this.reconnectTimeout = undefined;
        this.retry(getToken, userId, tranDauId);
      });
    }, 5000);
  }

  /**
   * ✅ Đăng ký lắng nghe topic battle
   */
  /**
   * Subscribe vào topic & queue của trận đấu
   */
  // subscribeBattle(tranDauId: number, callback: (ev: BattleEvent) => void) {
  //   if (!this.client || !this.client.connected) {
  //     console.warn('⚠️ STOMP chưa sẵn sàng để subscribe');
  //     return;
  //   }
  //
  //   // 1️⃣ Nhận event chung toàn phòng
  //   this.client.subscribe(`/topic/battle.${tranDauId}`, (message: IMessage) => {
  //     const body = JSON.parse(message.body);
  //     callback(body);
  //   });
  //
  //   // 2️⃣ Nhận event cá nhân (SCORE_UPDATE, PRIVATE NOTIFY, v.v.)
  //   this.client.subscribe(`/user/queue/battle`, (message: IMessage) => {
  //     const body = JSON.parse(message.body);
  //     callback(body);
  //   });
  //
  //   console.log(`📡 Đã subscribe /topic/battle.${tranDauId} & /user/queue/battle`);
  // }

  subscribeBattle(tranDauId: number, cb: (ev: any) => void) {
    const topic = `/topic/battle.${tranDauId}`;
    this.unsubscribe(topic);
    const sub = this.client!.subscribe(topic, (msg: IMessage) => {
      try {
        const payload = JSON.parse(msg.body);
        this.zone.run(() => cb(payload));
      } catch (err) {
        console.error('❌ WS parse error:', err);
      }
    });
    this.subs[topic] = sub;
  }

  unsubscribe(topic: string) {
    if (this.subs[topic]) {
      try {
        this.subs[topic].unsubscribe();
      } catch {
      }
      delete this.subs[topic];
    }
  }

  disconnect() {
    Object.keys(this.subs).forEach((k) => this.unsubscribe(k));
    this.client?.deactivate();
    this.client = undefined;
  }
}
