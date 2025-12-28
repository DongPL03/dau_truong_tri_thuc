import { CommonModule } from '@angular/common';
import { Component, computed, effect, inject, OnDestroy, OnInit, signal } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { RoiTranDauDTO } from '../../../dtos/tran-dau/roitran-dto';
import { SubmitAnswerDTO } from '../../../dtos/tran-dau/submitanswer-dto';
import { ThamGiaTranDauDTO } from '../../../dtos/tran-dau/thamgiatrandau-dto';
import { environment } from '../../../environments/environment';
import { ResponseObject } from '../../../responses/response-object';
import { FinishedPlayer } from '../../../responses/trandau/finished-player';
import { LichSuTranDauResponse } from '../../../responses/trandau/lichsutrandau';
import { NguoiChoiTrongPhongResponse } from '../../../responses/trandau/nguoi-choi-trong-phong-response';
import { SyncStateResponse } from '../../../responses/trandau/syncstate-response';
import { TranDauResponse } from '../../../responses/trandau/trandau-response';
import { BattleEvent } from '../../../services/ws-trandau.service';
import { Base } from '../../base/base';

import { PickerComponent } from '@ctrl/ngx-emoji-mart';
import { finalize } from 'rxjs/operators';
import {
  LoaiVatPham,
  SuDungVatPhamResponse,
  VatPhamInventory,
  VatPhamUtils,
} from '../../../models/vat-pham.model';
import { FriendSummaryResponse } from '../../../responses/banbe/friend_summary_response';
import { ChatMessage } from '../../../responses/nguoidung/chatmessage';
import { UserResponse } from '../../../responses/nguoidung/user-response';
import { UserSummaryResponse } from '../../../responses/nguoidung/user-summary-response';
import { VatPhamService } from '../../../services/vat-pham.service';

import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-chi-tiet-phong',
  imports: [CommonModule, FormsModule, PickerComponent, RouterLink],
  templateUrl: './chi-tiet-phong.html',
  styleUrl: './chi-tiet-phong.scss',
  standalone: true,
})
export class ChiTietPhong extends Base implements OnInit, OnDestroy {
  user?: UserResponse | null = null;
  avatarUrl: string = 'assets/images/default-profile-image.jpeg';
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/users/profile-images/';

  chatMessages = signal<ChatMessage[]>([]);
  chatInput = signal<string>('');

  showSummary = signal<boolean>(false);
  finalResult?: {
    winner: any;
    leaderboard: any[];
    myId: number;
  };
  // Thêm signal để điều khiển việc hiện/ẩn bảng emoji
  showEmojiPicker = signal<boolean>(false);

  preCountdown = signal<number>(0);
  private preCountdownTimer?: ReturnType<typeof setInterval>;

  loading = signal<boolean>(true);
  saving = signal<boolean>(false);

  battle = signal<TranDauResponse | null>(null);
  syncState = signal<SyncStateResponse | null>(null);

  leaderboard = signal<LeaderboardPlayer[]>([]);

  isHostUservariable = false;

  protected readonly environment = environment;
  protected readonly console = console;

  pageTitle = computed(() => this.battle()?.ten_phong ?? 'Phòng');
  status = computed(() => this.battle()?.trang_thai ?? 'PENDING');
  isPending = computed(() => this.status() === 'PENDING');
  isOngoing = computed(() => this.status() === 'ONGOING');
  isFinished = computed(() => this.status() === 'FINISHED');

  // join/submit local fields
  pinCode = signal<string>('');
  selectedAnswer = signal<'A' | 'B' | 'C' | 'D' | ''>('');

  // simple ticking countdown (client-side) derived from sync
  remainingSeconds = signal<number>(0);
  private timer?: ReturnType<typeof setInterval>;

  // ====== SUMMARY STATE ======
  mySummaryRow?: FinishedPlayer;
  isWinnerMe = false;

  // ====== CHỐNG NỘP NHIỀU LẦN ======
  submittedCurrentAnswer = signal<boolean>(false);

  onlineCount = signal<number>(0);

  localJoinedState = signal<boolean>(false);

  revealedCorrectAnswer = signal<string>(''); // "A" | "B" | "C" | "D" | ''
  revealedExplanation = signal<string>(''); // text giải thích

  // đã nằm trong class ChiTietPhong
  joinedBattle = signal<boolean>(false);

  show_invite_panel = false;
  invite_loading = false;
  invite_friends: FriendSummaryResponse[] = [];
  private inviting_ids = new Set<number>();

  // nếu bạn có sẵn base url avatar thì có thể dùng lại
  readonly default_avatar = 'assets/images/default-profile-image.jpeg';
  readonly image_base_url = 'http://localhost:8088/api/v1/users/profile-images/';

  currentCombo = signal<number>(0);

  reward_popup_shown = false;

  // Thêm vào đầu class
  showComboVFX = false;
  comboBonusPoints = 0; // Biến lưu điểm cộng thêm để hiển thị
  userSummary = signal<UserSummaryResponse | null>(null);

  // ================== POWER-UPS / ITEMS ==================
  private vatPhamService = inject(VatPhamService);
  inventory = signal<VatPhamInventory[]>([]);
  showItemPanel = signal<boolean>(false);
  activeMultiplier = signal<number>(1);
  eliminatedOptions = signal<string[]>([]);
  hasShield = signal<boolean>(false);
  itemUsing = signal<boolean>(false);
  protected readonly LoaiVatPham = LoaiVatPham;
  protected readonly VatPhamUtils = VatPhamUtils;

  // Danh sách người chơi trong phòng (trước khi trận đấu bắt đầu)
  playersInRoom = signal<NguoiChoiTrongPhongResponse[]>([]);

  // ================== USER MODAL (xem profile trong modal) ==================
  show_user_modal = false;
  user_modal_loading = false;
  user_modal_summary?: UserSummaryResponse | null;
  user_history_items: LichSuTranDauResponse[] = [];
  user_history_loading = false;

  constructor() {
    super();
    // Kiểm tra xem có cờ 'joined' được gửi từ PhongCho sang không
    const nav = this.router.currentNavigation();
    if (nav?.extras?.state?.['joined']) {
      console.log('🚀 Đã verify PIN từ phòng chờ, set trạng thái đã tham gia.');
      this.localJoinedState.set(true);
    }
    effect(() => {
      const s = this.syncState();
      if (!s || s.current_question_index < 0) {
        this.clearTimer();
        this.remainingSeconds.set(0);
        return;
      }
      // tránh gọi tick nếu chưa có thời gian bắt đầu
      const startIso = s.current_question_start ? Date.parse(s.current_question_start) : NaN;
      if (isNaN(startIso)) {
        console.warn(
          '⏸️ Bỏ qua effect tick() vì current_question_start chưa hợp lệ:',
          s.current_question_start
        );
        return;
      }
      const endAt = startIso + s.seconds_per_question * 1000;
      // nếu timer đã chạy rồi, không khởi động lại nữa
      if (this.timer) {
        return;
      }
      console.log('💡 Effect tick() được kích hoạt cho câu', s.current_question_index + 1);
      this.tick(endAt);
    });
  }

  ngOnInit(): void {
    const id = Number(this.route.snapshot.paramMap.get('id'));
    if (!id) return;

    const token = this.tokenService.getAccessToken();
    const user = this.userService.currentUser();
    if (!user) return;

    this.wsTrandauService
      .connect(() => token, user.id, id)
      .then(() => {
        console.log('✅ WebSocket connected!');
        this.wsTrandauService.subscribeBattle(id, (ev) => this.handleBattleEvent(ev));
      })
      .catch((err) => console.error('❌ WebSocket connect failed:', err));

    // Lấy dữ liệu lần đầu
    this.fetchDetail(id, () => this.doSync());

    [500, 1500, 3000].forEach((time) => {
      setTimeout(() => {
        console.log(`🔄 [${time}ms] Đang gọi lại API để check số người...`);
        this.refreshRoomInfo();
      }, time);
    });
    this.currentUserName();
    setTimeout(() => this.syncState.update((s) => (s ? { ...s } : s)), 200);
    this.loadUserInfo();
    this.loadInventory();

    // 🔹 Load danh sách người chơi trong phòng (cho trạng thái PENDING)
    this.loadPlayersInRoom(id);
  }

  private loadUserInfo(): void {
    this.user = this.userService.getUserResponseFromLocalStorage();
    if (this.user?.avatar_url) {
      this.avatarUrl = this.imageBaseUrl + this.user.avatar_url;
      console.log('Avatar URL:', this.avatarUrl);
    } else {
      this.avatarUrl = 'assets/images/default-profile-image.jpeg';
    }
  }

  /**
   * 🔹 Lấy danh sách người chơi trong phòng (dùng cho trạng thái PENDING)
   */
  private loadPlayersInRoom(battleId: number): void {
    this.tranDauService.getPlayersInRoom(battleId).subscribe({
      next: (res) => {
        console.log('👥 Danh sách người chơi trong phòng:', res.data);
        if (res.data) {
          this.playersInRoom.set(res.data);
          this.onlineCount.set(res.data.length);
        }
      },
      error: (err) => {
        console.error('❌ Lỗi khi lấy danh sách người chơi:', err);
      },
    });
  }

  ngOnDestroy() {
    this.clearTimer();
    this.wsTrandauService.disconnect();
  }

  // =====================================================
  // REST API
  // =====================================================

  fetchDetail(id: number, next?: () => void) {
    this.loading.set(true);
    this.tranDauService.getBattleDetail(id).subscribe({
      next: (res: ResponseObject<TranDauResponse>) => {
        const data = res.data!;
        this.battle.set(data);

        // ============================================================
        // 👇 THÊM ĐOẠN NÀY VÀO ĐỂ FIX LỖI
        // ============================================================

        // Lấy danh sách người chơi từ API và gán ngay vào leaderboard
        // Bạn cần kiểm tra xem backend trả về key tên là 'leaderboard' hay 'nguoi_tham_gia'
        const players = (data as any).leaderboard || (data as any).nguoi_tham_gia || [];

        console.log('Danh sách người chơi init:', players);
        this.leaderboard.set(players);

        // Sau khi dòng trên chạy, signal isJoined sẽ tự động tính lại -> thành TRUE
        // -> Ô nhập PIN sẽ biến mất ngay lập tức.
        // ============================================================

        this.loading.set(false);
        next?.();

        this.joinedBattle.set(!!data.da_tham_gia);

        // Cập nhật số lượng người online
        if ((data as any).so_luong_nguoi_tham_gia) {
          this.onlineCount.set((data as any).so_luong_nguoi_tham_gia);
        } else {
          this.onlineCount.set(players.length || 1);
        }
      },
      error: () => {
        this.loading.set(false);
        Swal.fire('Lỗi', 'Không thể tải thông tin phòng', 'error').then(() =>
          this.router.navigateByUrl('/home')
        );
      },
    });
  }

  loadUserSummary(user_id: number) {
    this.userService.getUserSummary(user_id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        console.log('✅ Thống kê người dùng tải về:', res.data);
        this.userSummary.set(res.data!);
      },
      error: (err) => {
        console.error('❌ Lỗi khi tải thống kê người dùng:', err);
      },
    });
  }

  doSync() {
    const id = this.battle()?.id;
    if (!id) return;
    this.tranDauService.sync(id).subscribe({
      next: (res: ResponseObject<SyncStateResponse>) => {
        this.syncState.set(res.data!);

        // ✅ ép effect chạy lại nếu có câu hỏi đầu tiên
        const s = res.data!;
      },
    });
  }

  // =====================================================
  // THAM GIA PHÒNG + KẾT NỐI WS
  // =====================================================

  isJoined = computed(() => {
    const myId = this.userService.getUserId();
    const players = this.leaderboard();

    // 👇 SỬA LẠI: Xóa bỏ "|| this.isHostUser()"
    // Logic đúng: Chỉ tính là đã join khi có tên trong danh sách HOẶC vừa nhập PIN xong
    return players.some((p) => p.user_id === myId) || this.localJoinedState();
  });

  // 3. Bổ sung hàm isHostUser cho chắc chắn (nếu chưa có logic chuẩn)
  isHostUser(): boolean {
    const b = this.battle();
    const u = this.userService.currentUser(); // Đảm bảo lấy đúng user hiện tại
    if (!b || !u) return false;
    // So sánh ID hoặc Tên tùy vào dữ liệu backend trả về
    // Tốt nhất là so sánh User ID nếu có, ở đây tạm dùng tên như code cũ của bạn
    return b.chu_phong_ten === u.ho_ten;
  }

  join() {
    const b = this.battle();
    if (!b) return;
    const dto: ThamGiaTranDauDTO = { tran_dau_id: b.id };
    if (!b.cong_khai) dto.ma_pin = this.pinCode();
    this.saving.set(true);
    this.tranDauService
      .joinBattle(dto as any)
      .pipe(finalize(() => this.saving.set(false)))
      .subscribe({
        next: () => {
          this.joinedBattle.set(true);
          Swal.fire('Thành công', 'Bạn đã tham gia phòng', 'success').then(() => {});

          this.localJoinedState.set(true);
          // ⬇️ Sau khi join xong, gọi lại detail để lấy đúng số người tham gia (lúc này DB đã là 2)
          this.refreshRoomInfo();
          this.doSync(); // giữ lại để lấy trạng thái câu hỏi
        },
        error: (e) => {
          // Nếu lỗi là "User đã tham gia", ta coi như thành công
          if (e?.error?.message?.includes('đã tham gia')) {
            Swal.fire('Đã tham gia', 'Bạn đã ở trong phòng này rồi', 'info').then(() => {});
            this.refreshRoomInfo();
            this.doSync();
          } else {
            Swal.fire(
              'Không thể tham gia',
              e?.error?.message || 'Vui lòng kiểm tra lại',
              'error'
            ).then(() => {});
          }
        },
      });
  }

  leave() {
    // 1. Hỏi xác nhận trước cho chắc ăn
    Swal.fire({
      title: 'Rời phòng?',
      text: 'Bạn có chắc muốn thoát không?',
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Rời đi',
      cancelButtonText: 'Ở lại',
    }).then((r) => {
      if (!r.isConfirmed) return;

      // =========================================================
      // 👇 LOGIC MỚI: KIỂM TRA XEM ĐÃ JOIN CHƯA
      // =========================================================

      // TRƯỜNG HỢP 1: Chưa tham gia (đang xem) -> Chỉ cần chuyển trang về Home
      if (!this.isJoined()) {
        this.wsTrandauService.disconnect(); // Ngắt kết nối socket cho sạch
        this.router.navigateByUrl('/home').then((r) => {});
        return; // Dừng hàm tại đây, không gọi API bên dưới
      }

      // TRƯỜNG HỢP 2: Đã tham gia -> Gọi API để Backend xóa tên khỏi danh sách
      const b = this.battle();
      if (!b) return;

      const dto: RoiTranDauDTO = { tran_dau_id: b.id };
      this.saving.set(true);

      this.tranDauService.leaveBattle(dto as any).subscribe({
        next: () => {
          this.saving.set(false);
          Swal.fire('Đã rời phòng', '', 'success').then(() => {});
          this.wsTrandauService.disconnect();
          this.router.navigateByUrl('/home').then((r) => {});
        },
        error: (e) => {
          this.saving.set(false);
          // Dù lỗi API (do mạng lag hay gì đó) thì cũng nên cho người dùng thoát ra
          // Nếu muốn chặt chẽ thì giữ alert, nếu muốn UX mượt thì navigate luôn
          Swal.fire('Lỗi', e?.error?.message || 'Không thể rời phòng', 'error').then((r) => {});

          // Option: Nếu API lỗi "Bạn chưa ở trong phòng", ta vẫn cho họ về Home luôn
          if (e?.error?.message?.includes('chưa ở trong phòng')) {
            this.router.navigateByUrl('/home').then((r) => {});
          }
        },
      });
    });
  }

  // =====================================================
  // WEBSOCKET EVENT HANDLER
  // =====================================================
  handleBattleEvent(evt: BattleEvent) {
    console.log('📡 WS Event:', evt);
    switch (evt.type) {
      case 'PLAYER_JOINED': {
        this.leaderboard.update((list) =>
          list.map((p) => (p.user_id === evt.user_id ? { ...p, da_roi: false } : p))
        );
        Swal.fire('👋 Người chơi mới', `${evt.ho_ten} vừa tham gia phòng`, 'info').then((r) => {});
        this.refreshRoomInfo();
        // 🔹 Refresh danh sách người chơi trong phòng
        const battleId = this.battle()?.id;
        if (battleId) this.loadPlayersInRoom(battleId);
        break;
      }
      case 'PLAYER_LEFT': {
        // Đánh dấu "đã rời trận" trên leaderboard
        this.leaderboard.update((list) =>
          list.map((p) => (p.user_id === evt.user_id ? { ...p, da_roi: true } : p))
        );
        Swal.fire('🚪 Người chơi rời đi', `${evt.ho_ten} đã rời phòng`, 'warning').then((r) => {});
        this.refreshRoomInfo();
        // 🔹 Refresh danh sách người chơi trong phòng
        const leftBattleId = this.battle()?.id;
        if (leftBattleId) this.loadPlayersInRoom(leftBattleId);
        break;
      }
      case 'BATTLE_STARTED':
        Swal.fire({
          icon: 'success',
          title: 'Trận đấu bắt đầu!',
          text: `Phòng: ${evt.ten_phong} (${evt.tong_cau_hoi} câu hỏi, ${evt.thoi_gian_moi_cau_giay}s mỗi câu)`,
          timer: 1800,
          showConfirmButton: false,
        }).then((r) => {});

        this.battle.update((b) => ({
          ...b!,
          trang_thai: 'ONGOING',
          bat_dau_luc: evt.bat_dau_luc,
        }));

        const pre = (evt as any).dem_nguoc_truoc_giay ?? 0;
        if (pre > 0) {
          this.startPreCountdown(pre);
        }

        // ✅ Gọi sync 2 lần an toàn để chắc chắn nhận câu đầu tiên
        const doInitSync = () => {
          this.tranDauService.sync(evt.tran_dau_id).subscribe({
            next: (res: ResponseObject<SyncStateResponse>) => {
              const s = res.data!;
              this.syncState.set(s);
            },
          });
        };
        setTimeout(doInitSync, 200); // lần đầu sync
        setTimeout(doInitSync, 700); // lần hai backup nếu backend gửi trễ
        break;

      case 'NEW_QUESTION': {
        const q = evt.question;
        if (!q) return;

        this.revealedCorrectAnswer.set('');
        this.revealedExplanation.set('');
        // Reset hiệu ứng power-ups từ câu trước
        this.resetItemEffects();

        const newState = {
          tran_dau_id: evt.tran_dau_id,
          current_question_index: evt.question_index,
          current_question_id: q.id,
          seconds_per_question: evt.thoi_gian_cau_giay,
          current_question_start: evt.timestamp,
          noi_dung: q.noi_dung,
          loai_noi_dung: q.loai_noi_dung,
          duong_dan_tep: q.duong_dan_tep,
          a: q.lua_chon_a,
          b: q.lua_chon_b,
          c: q.lua_chon_c,
          d: q.lua_chon_d,
          my_total_points: this.syncState()?.my_total_points ?? 0,
          _version: Math.random(),
        };

        this.syncState.set({ ...newState });

        // ⛔ reset trạng thái nộp của câu mới
        this.submittedCurrentAnswer.set(false);
        this.selectedAnswer.set('');

        this.submittedCurrentAnswer.set(false);
        this.selectedAnswer.set('');

        const startMs = Date.parse(newState.current_question_start);
        if (isNaN(startMs)) {
          console.warn('⚠️ current_question_start không hợp lệ:', newState.current_question_start);
          return;
        }
        this.clearTimer();
        this.tick(startMs + newState.seconds_per_question * 1000);
        break;
      }
      case 'ANSWER_REVEAL': {
        // Lưu đáp án đúng & giải thích
        this.revealedCorrectAnswer.set(evt.dap_an_dung); // "A" | "B" | "C" | "D"
        this.revealedExplanation.set(evt.giai_thich || '');
        break;
      }
      case 'SCORE_UPDATE':
        const myId = this.userService.getUserId();
        if (evt.user_id !== myId) return;
        const combo = evt.combo_streak ?? 0;
        this.currentCombo.set(combo);
        const pointsBonus = evt.combo_bonus || 0;
        const pointsGained = evt.gained_points || 0;
        this.comboBonusPoints = pointsGained;
        if (evt.correct) {
          if (combo >= 2) {
            this.triggerComboVFX();
          } else {
            Swal.fire({
              icon: 'success',
              title: `+${pointsGained} điểm`,
              toast: true,
              position: 'top',
              showConfirmButton: false,
              timer: 1200,
              background: '#dcfce7',
              color: '#166534',
            }).then((r) => {});
          }
        } else {
          Swal.fire({
            icon: 'error',
            title: 'Sai rồi!',
            text: 'Tiếc quá!',
            toast: true,
            position: 'top',
            showConfirmButton: false,
            timer: 1500,
            background: '#fee2e2', // Nền đỏ nhạt
            color: '#991b1b',
          }).then((r) => {});
        }
        setTimeout(() => {
          this.syncState.update((s) => (s ? { ...s, my_total_points: evt.total_points } : s));
        }, 300);
        break;

      case 'LEADERBOARD_UPDATE':
        // @ts-ignore
        this.leaderboard.set(evt.players || []);
        if (evt.players && evt.players.length > 0) {
          this.onlineCount.set(evt.players.length);
        }
        break;

      case 'FINISHED': {
        this.battle.set({ ...(this.battle() as TranDauResponse), trang_thai: 'FINISHED' });
        const myId = this.userService.getUserId();
        this.finalResult = {
          winner: evt.winner,
          leaderboard: evt.leaderboard as FinishedPlayer[],
          myId,
        };
        this.mySummaryRow = this.finalResult.leaderboard.find((p) => p.user_id === myId);
        console.log('🏆 Dòng kết quả của tôi:', this.mySummaryRow);
        this.isWinnerMe = !!(this.finalResult.winner && this.finalResult.winner.user_id === myId);
        this.showSummary.set(true);
        this.clearTimer();
        if (this.user?.id) {
          this.loadUserSummary(this.user.id);
        }
        setTimeout(() => {
          this.show_match_reward_popup();
        }, 1000);
        break;
      }

      case 'CHAT_MESSAGE': {
        const meId = this.userService.getUserId();
        const msg: ChatMessage = {
          user_id: evt.user_id,
          ho_ten: evt.ho_ten,
          noi_dung: evt.noi_dung,
          is_system: (evt as any).is_system ?? false,
          timestamp: evt.timestamp,
          is_me: evt.user_id === meId,
        };
        this.chatMessages.update((list) => [...list, msg]);
        setTimeout(() => {
          const box = document.querySelector('.chat-messages');
          if (box) {
            (box as HTMLElement).scrollTop = (box as HTMLElement).scrollHeight;
          }
        }, 50);
        break;
      }

      // ================== POWER-UPS / ITEMS EVENTS ==================
      case 'ITEM_USED': {
        const data = (evt as any).data || evt;
        const myId = this.userService.getUserId();

        // Nếu không phải mình thì chỉ hiển thị toast
        if (data.user_id !== myId) {
          Swal.fire({
            toast: true,
            position: 'top',
            icon: 'info',
            title: `${data.ho_ten} đã sử dụng ${data.ten_vat_pham || '🎁 Vật phẩm'}`,
            timer: 2000,
            showConfirmButton: false,
          });
        }

        // Xử lý hiệu ứng 50/50 từ người khác (nếu cần show)
        if (data.hieu_ung?.dap_an_bi_loai && data.user_id === myId) {
          this.eliminatedOptions.set(data.hieu_ung.dap_an_bi_loai);
        }
        break;
      }

      case 'EFFECT_50_50': {
        const myId = this.userService.getUserId();
        if ((evt as any).user_id === myId) {
          this.eliminatedOptions.set((evt as any).dap_an_bi_loai || []);
        }
        break;
      }

      case 'MULTIPLIER_ACTIVE': {
        const myId = this.userService.getUserId();
        if ((evt as any).user_id === myId) {
          this.activeMultiplier.set((evt as any).multiplier || 1);
        } else {
          // Hiển thị thông báo người khác đang có boost
          Swal.fire({
            toast: true,
            position: 'top',
            icon: 'warning',
            title: `⚡ ${(evt as any).ho_ten} kích hoạt x${(evt as any).multiplier} điểm!`,
            timer: 1500,
            showConfirmButton: false,
          });
        }
        break;
      }
    }
  }

  startBattle() {
    const id = this.battle()?.id;
    if (!id) return;
    this.saving.set(true);
    this.tranDauService.startBattle(id).subscribe({
      next: (res) => {
        this.saving.set(false);
        this.battle.set({ ...(this.battle() as TranDauResponse), trang_thai: 'ONGOING' });
        Swal.fire('Bắt đầu!', 'Trận đấu đã bắt đầu', 'success').then((r) => {});
        this.doSync();
      },
      error: (e) => {
        this.saving.set(false);
        Swal.fire(
          'Không thể bắt đầu',
          e?.error?.message || 'Bạn có quyền chủ phòng?',
          'error'
        ).then((r) => {});
      },
    });
  }

  finishBattle() {
    const id = this.battle()?.id;
    if (!id) return;
    Swal.fire({
      title: 'Kết thúc trận?',
      text: 'Hệ thống sẽ chốt điểm & phát kết quả',
      icon: 'warning',
      showCancelButton: true,
    }).then((r) => {
      if (!r.isConfirmed) return;
      this.saving.set(true);
      this.tranDauService.finishBattle(id).subscribe({
        next: () => {
          this.saving.set(false);
          this.battle.set({ ...(this.battle() as TranDauResponse), trang_thai: 'FINISHED' });
          Swal.fire('Đã kết thúc', 'Xem bảng xếp hạng ở phía dưới', 'success').then((r) => {});
          this.doSync();
        },
        error: (e) => {
          this.saving.set(false);
          Swal.fire('Không thể kết thúc', e?.error?.message || 'Thử lại sau', 'error').then(
            (r) => {}
          );
        },
      });
    });
  }

  submitSelectedAnswer() {
    const s = this.syncState();
    const b = this.battle();
    if (!s || !b || s.current_question_index < 0 || !s.current_question_id) return;

    // ⛔ đã nộp câu hiện tại rồi
    if (this.submittedCurrentAnswer()) {
      Swal.fire('Bạn đã nộp đáp án', 'Hãy chờ câu hỏi tiếp theo nhé', 'info').then(() => {});
      return;
    }

    const ans = this.selectedAnswer();
    if (!ans) {
      Swal.fire('Chưa chọn đáp án', 'Hãy chọn A/B/C/D', 'info').then((r) => {});
      return;
    }

    const dto: SubmitAnswerDTO = {
      tran_dau_id: b.id,
      cau_hoi_id: s.current_question_id,
      answer: ans,
    };
    // ✅ Đánh dấu đã nộp NGAY LẬP TỨC để chặn double-click
    this.submittedCurrentAnswer.set(true);
    this.saving.set(true);
    this.tranDauService.submitAnswer(dto as any).subscribe({
      next: (res) => {
        this.saving.set(false);
        // Đánh dấu đã nộp
        this.submittedCurrentAnswer.set(true);
      },
      error: (e) => {
        this.saving.set(false);
        // Nếu lỗi thì cho phép nộp lại
        this.submittedCurrentAnswer.set(false);
        Swal.fire('Không thể nộp đáp án', e?.error?.message || 'Thử lại sau', 'error').then(
          (r) => {}
        );
      },
    });
  }

  sendChat() {
    // 1. Lấy dữ liệu và Validate đầu vào
    const content = this.chatInput().trim();
    const battleId = this.battle()?.id;

    if (!content || !battleId) return;

    // 2. Bật trạng thái loading
    this.saving.set(true);

    // 3. Gọi API
    this.tranDauService
      .sendChat({ tran_dau_id: battleId, noi_dung: content } as any)
      .pipe(
        // Dùng finalize để luôn tắt loading dù thành công hay thất bại
        finalize(() => this.saving.set(false))
      )
      .subscribe({
        next: () => {
          // 4. Reset input & Đóng Emoji Picker (UX)
          this.chatInput.set('');
          this.showEmojiPicker.set(false);

          // (Tùy chọn) Focus lại vào ô input để chat tiếp
          // document.querySelector<HTMLInputElement>('input[name="chat_input"]')?.focus();
        },
        error: (err) => {
          // 5. Xử lý lỗi
          console.error('Chat error:', err);
          // Toast nhỏ gọn thay vì Alert to đùng (Optional)
          Swal.fire({
            icon: 'error',
            title: 'Không gửi được',
            text: err?.error?.message || 'Vui lòng thử lại',
            toast: true,
            position: 'top-end',
            showConfirmButton: false,
            timer: 3000,
          }).then((r) => {});
        },
      });
  }

  // =====================================================
  // TIMER
  // =====================================================

  tick(endAtMs: number) {
    if (!endAtMs || isNaN(endAtMs)) {
      console.warn('⏱️ Bỏ qua tick() vì endAtMs không hợp lệ:', endAtMs);
      return;
    }
    this.clearTimer();
    const run = () => {
      const remain = Math.max(0, Math.floor((endAtMs - Date.now()) / 1000));
      this.remainingSeconds.set(remain);
      if (remain <= 0) this.clearTimer();
    };

    run();
    this.timer = setInterval(run, 1000);
  }

  clearTimer() {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = undefined;
    }
  }

  badgeClass(status: string | undefined) {
    switch (status) {
      case 'PENDING':
        return 'badge pending';
      case 'ONGOING':
        return 'badge ongoing';
      case 'FINISHED':
        return 'badge finished';
      default:
        return 'badge';
    }
  }

  // Thêm hàm này vào trong class ChiTietPhong
  refreshRoomInfo() {
    const id = this.battle()?.id;
    if (!id) return;

    this.tranDauService.getBattleDetail(id).subscribe({
      next: (res: ResponseObject<TranDauResponse>) => {
        const data = res.data!;
        this.battle.set(data);

        // 🔍 LOG DATA RA ĐỂ KIỂM TRA XEM BACKEND TRẢ VỀ CÁI GÌ
        console.log('🔍 Full Data từ API:', data);

        // Xử lý linh hoạt tên biến (Backend có thể trả về camelCase hoặc snake_case)
        let count = 0;

        if ('so_luong_nguoi_tham_gia' in data) {
          count = (data as any).so_luong_nguoi_tham_gia;
        } else if ('soLuongNguoiThamGia' in data) {
          count = (data as any).soLuongNguoiThamGia;
        }
        // Fallback: Nếu API không trả về số lượng, dùng độ dài danh sách leaderboard/nguoi_tham_gia nếu có
        else if ((data as any).leaderboard?.length > 0) {
          count = (data as any).leaderboard.length;
        }

        console.log('📊 Số người chốt lại là:', count);

        // Chỉ cập nhật nếu count hợp lệ (> 0)
        if (count > 0) {
          this.onlineCount.set(count);
        }
      },
      error: (err) => console.error('Lỗi refresh room info', err),
    });
  }

  // Hàm toggle
  toggleEmojiPicker() {
    this.showEmojiPicker.update((v) => !v);
  }

  // Hàm xử lý khi chọn 1 emoji
  addEmoji(event: any) {
    const emoji = event.emoji.native;
    // Nối emoji vào chuỗi input hiện tại
    this.chatInput.update((current) => current + emoji);
    // (Tùy chọn) Đóng bảng sau khi chọn xong nếu muốn
    // this.showEmojiPicker.set(false);
  }

  // Trong class ChiTietPhong

  getAvatarColor(name: string): string {
    const colors = [
      '#ef4444',
      '#f97316',
      '#f59e0b',
      '#84cc16',
      '#10b981',
      '#06b6d4',
      '#3b82f6',
      '#6366f1',
      '#8b5cf6',
      '#d946ef',
      '#f43f5e',
    ];
    let hash = 0;
    for (let i = 0; i < name.length; i++) {
      hash = name.charCodeAt(i) + ((hash << 5) - hash);
    }
    const index = Math.abs(hash % colors.length);
    return colors[index];
  }

  selectAnswer(opt: string) {
    this.selectedAnswer.set(opt as any);
  }

  is_room_full = computed(() => {
    const b = this.battle();
    if (!b) return false;
    return this.onlineCount() >= (b.gioi_han_nguoi_choi ?? 0);
  });

  can_join = computed(() => {
    return this.isPending() && !this.is_room_full();
  });

  goBackToBattleList() {
    this.router.navigateByUrl('/tran-dau/pending').then((r) => {}); // hoặc '/battle/danh-sach-bo-cau-hoi'
  }

  goBackHome() {
    this.router.navigateByUrl('/home').then((r) => {});
  }

  get summary_leaderboard(): FinishedPlayer[] {
    if (!this.finalResult?.leaderboard) {
      return [];
    }
    // đảm bảo sort theo xếp_hang
    return [...this.finalResult.leaderboard].sort((a, b) => a.xep_hang - b.xep_hang);
  }

  get summary_winner(): FinishedPlayer | null {
    // kiểu ở Winner trong WS gần như giống FinishedPlayer, mình ép về cho đồng nhất
    const w = this.finalResult?.winner as FinishedPlayer | undefined;
    return w ?? null;
  }

  is_my_row(p: FinishedPlayer): boolean {
    return this.finalResult?.myId === p.user_id;
  }

  is_winner_row(p: FinishedPlayer): boolean {
    return this.finalResult?.winner?.user_id === p.user_id;
  }

  practiceThisSet() {
    const b = this.battle();
    if (!b || !b.bo_cau_hoi_id) {
      // phòng không có bộ câu hỏi thì thôi
      return;
    }

    const boId = b.bo_cau_hoi_id;

    this.router
      .navigate(['/luyen-tap'], {
        queryParams: {
          bo_cau_hoi_id: boId,
        },
      })
      .then((r) => {});
  }

  startPreCountdown(seconds: number) {
    if (this.preCountdownTimer) {
      clearInterval(this.preCountdownTimer);
    }
    this.preCountdown.set(seconds);

    this.preCountdownTimer = setInterval(() => {
      const cur = this.preCountdown();
      if (cur <= 1) {
        this.preCountdown.set(0);
        clearInterval(this.preCountdownTimer!);
        this.preCountdownTimer = undefined;
      } else {
        this.preCountdown.set(cur - 1);
      }
    }, 1000);
  }

  currentUserName() {
    const u = this.userService.currentUser();
    return u ? u.ho_ten : 'Người chơi';
  }

  // Trong class ChiTietPhong

  // 1. Thêm computed này vào
  hasLongAnswer = computed(() => {
    const s = this.syncState();
    if (!s) return false;

    const threshold = 25; // ⚡ Ngưỡng ký tự. Nếu dài hơn số này -> chuyển thành 1 cột

    // Kiểm tra độ dài của cả 4 đáp án
    return (
      (s.a || '').length > threshold ||
      (s.b || '').length > threshold ||
      (s.c || '').length > threshold ||
      (s.d || '').length > threshold
    );
  });

  open_invite_panel(): void {
    this.show_invite_panel = true;

    // Chỉ load 1 lần, hoặc bạn thích thì luôn reload
    if (this.invite_friends.length === 0) {
      this.load_friends_for_invite();
    }
  }

  close_invite_panel(): void {
    this.show_invite_panel = false;
  }

  private load_friends_for_invite(): void {
    this.invite_loading = true;

    this.friendService.getFriends().subscribe({
      next: (res: ResponseObject<FriendSummaryResponse[]>) => {
        this.invite_friends = res.data || [];
        this.invite_loading = false;
      },
      error: () => {
        this.invite_loading = false;
        this.invite_friends = [];
      },
    });
  }

  build_friend_avatar(avatar_url?: string | null): string {
    if (!avatar_url) {
      return this.default_avatar;
    }
    return this.image_base_url + avatar_url;
  }

  is_inviting(user_id: number): boolean {
    return this.inviting_ids.has(user_id);
  }

  invite_friend_to_battle(friend: FriendSummaryResponse): void {
    // Lấy id trận đấu hiện tại – chỉnh lại cho đúng field của bạn
    const battle = this.battle?.(); // nếu bạn đang dùng signal
    if (!battle) {
      return;
    }
    const tran_dau_id = battle.id as number;

    this.inviting_ids.add(friend.user_id);

    this.tranDauService.inviteFriend(tran_dau_id, friend.user_id).subscribe({
      next: () => {
        this.inviting_ids.delete(friend.user_id);
        // Có thể popup nhỏ: “Đã gửi lời mời cho XXX”
      },
      error: () => {
        this.inviting_ids.delete(friend.user_id);
      },
    });
  }

  private show_match_reward_popup(): void {
    const row = this.mySummaryRow;
    // Lấy dữ liệu từ signal userSummary vừa load được
    const summary = this.userSummary();

    if (!row || !summary || this.reward_popup_shown) {
      return;
    }
    this.reward_popup_shown = true;

    const xpGained = row.xp_gained ?? 0;
    const goldGained = row.gold_gained ?? 0;

    // --- TÍNH TOÁN THANH XP (Dựa trên logic trang Home của bạn) ---
    const currentXP = summary.xp_in_current_level;
    const remainingXP = summary.xp_next_level;
    const totalLevelXP = currentXP + remainingXP; // Tổng XP cần của level hiện tại

    // 1. Phần trăm hiện tại (Sau khi đã cộng)
    // Nếu totalLevelXP = 0 (tránh chia cho 0) thì set là 100%
    const percentNew = totalLevelXP > 0 ? (currentXP / totalLevelXP) * 100 : 100;

    // 2. Phần trăm cũ (Trước khi cộng)
    // Nếu vừa lên cấp (level_after > level_before), coi như thanh cũ là 0% để chạy từ đầu cho đẹp
    const isLevelUp = (row.level_after ?? 0) > (row.level_before ?? 0);

    let percentOld = 0;
    let percentGainedWidth = 0;

    if (isLevelUp) {
      // Trường hợp Lên cấp:
      // Thanh cũ = 0%, Thanh mới chạy từ 0 -> percentNew
      percentOld = 0;
      percentGainedWidth = percentNew;
    } else {
      // Trường hợp bình thường:
      // Tính XP trước đó = XP hiện tại - XP vừa nhận
      const xpBefore = Math.max(0, currentXP - xpGained);
      percentOld = totalLevelXP > 0 ? (xpBefore / totalLevelXP) * 100 : 0;

      // Độ rộng của đoạn XP vừa nhận
      percentGainedWidth = percentNew - percentOld;
    }

    // --- RENDER HTML ---
    let htmlContent = `
    <div class="victory-card-container">

      <div class="victory-header-cartoon">
        <img src="${
          this.isWinnerMe
            ? 'https://cdn-icons-png.flaticon.com/512/2583/2583344.png'
            : 'https://cdn-icons-png.flaticon.com/512/1055/1055666.png'
        }"
          class="victory-icon-img">
      </div>

      <div class="victory-title-cartoon">${this.isWinnerMe ? 'VICTORY' : 'COMPLETED'}</div>
      <div style="margin-bottom: 20px; color: #cbd5e1; font-size: 0.9rem;">
        ${isLevelUp ? 'Chúc mừng bạn đã lên cấp mới!' : 'Bạn đã làm rất tốt!'}
      </div>

      <div class="rewards-cartoon-row">
        <div class="r-item">
          <span class="r-icon">⚡</span>
          <span class="r-val xp-txt">+${xpGained}</span>
          <span class="r-label">Kinh nghiệm</span>
        </div>
        <div class="r-item">
          <span class="r-icon">🪙</span>
          <span class="r-val gold-txt">+${goldGained}</span>
          <span class="r-label">Vàng</span>
        </div>
      </div>

      <div class="xp-bar-wrapper">
        <div class="xp-bar-labels">
          <span>Level ${summary.level}</span>
          <span class="xp-val">${currentXP} / ${totalLevelXP} XP</span>
        </div>

        <div class="progress-track">
          <div class="progress-fill-old" style="width: ${percentOld}%"></div>

          <div id="anim-xp-new" class="progress-fill-new"
               style="width: 0%; left: ${percentOld}%">
          </div>
        </div>

        <div style="text-align:right; font-size:10px; color:#fbbf24; margin-top:4px; font-weight:bold;">
          +${xpGained} XP vừa nhận!
        </div>
      </div>

    </div>
  `;

    Swal.fire({
      html: htmlContent,
      showConfirmButton: true,
      confirmButtonText: 'NHẬN QUÀ NGAY',
      background: 'transparent',
      backdrop: `rgba(15, 23, 42, 0.9)`,
      customClass: {
        confirmButton: 'btn-cartoon-ok',
        popup: 'game-victory-popup',
      },
      didOpen: () => {
        // Kích hoạt Animation sau 300ms
        setTimeout(() => {
          const bar = document.getElementById('anim-xp-new');
          if (bar) {
            // Set width thực tế để CSS transition chạy
            bar.style.width = `${percentGainedWidth}%`;
          }
        }, 300);
      },
    }).then(() => {});
  }

  triggerComboVFX() {
    this.showComboVFX = true;
    // Tự động tắt sau 1.5 giây
    setTimeout(() => {
      this.showComboVFX = false;
    }, 1500);
  }

  // Thêm các getter này vào class ChiTietPhong

  // Lấy Top 3 để đưa lên bục
  get topThree() {
    const list = this.summary_leaderboard;
    // Đảm bảo mảng đủ 3 phần tử (để render slot trống nếu ít người)
    return [
      list[0] || null, // Top 1
      list[1] || null, // Top 2
      list[2] || null, // Top 3
    ];
  }

  // Lấy danh sách còn lại (từ hạng 4 trở đi)
  get restPlayers() {
    return this.summary_leaderboard.slice(3);
  }

  getAvatarUrl(user_id: number) {
    const player = this.leaderboard().find((p) => p.user_id === user_id);
    if (player && player.avatar_url) {
      return this.image_base_url + player.avatar_url;
    }
    return this.default_avatar;
  }

  // ================== POWER-UPS / ITEMS METHODS ==================

  /**
   * Load inventory của user
   */
  loadInventory(): void {
    this.vatPhamService.getInventory().subscribe({
      next: (items) => {
        this.inventory.set(items);
        console.log('📦 Inventory loaded:', items.length, 'items');
      },
      error: (err) => console.error('❌ Error loading inventory:', err),
    });
  }

  /**
   * Toggle hiển thị panel vật phẩm
   */
  toggleItemPanel(): void {
    this.showItemPanel.update((v) => !v);
  }

  /**
   * Sử dụng vật phẩm
   */
  useItem(item: VatPhamInventory): void {
    if (this.itemUsing() || item.so_luong <= 0) return;

    const battleId = this.battle()?.id;
    const questionIndex = this.syncState()?.current_question_index;

    if (!battleId) {
      Swal.fire('Lỗi', 'Không tìm thấy trận đấu', 'error');
      return;
    }

    // Confirm với Swal nếu là item quý hiếm
    if (item.do_hiem === 'LEGENDARY' || item.do_hiem === 'EPIC') {
      Swal.fire({
        title: `Sử dụng ${item.ten}?`,
        html: `<p>${item.mo_ta}</p><p class="text-warning">Bạn chỉ còn ${item.so_luong} vật phẩm này!</p>`,
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: 'Sử dụng',
        cancelButtonText: 'Hủy',
      }).then((result) => {
        if (result.isConfirmed) {
          this.executeUseItem(battleId, item, questionIndex);
        }
      });
    } else {
      this.executeUseItem(battleId, item, questionIndex);
    }
  }

  /**
   * Thực hiện sử dụng vật phẩm
   */
  private executeUseItem(battleId: number, item: VatPhamInventory, questionIndex?: number): void {
    this.itemUsing.set(true);

    this.vatPhamService.useItemByType(battleId, item.loai, questionIndex).subscribe({
      next: (response: SuDungVatPhamResponse) => {
        this.itemUsing.set(false);

        if (response.thanh_cong) {
          // Cập nhật inventory
          this.inventory.update((inv) =>
            inv
              .map((i) =>
                i.loai === item.loai ? { ...i, so_luong: response.so_luong_con_lai } : i
              )
              .filter((i) => i.so_luong > 0)
          );

          // Áp dụng hiệu ứng UI
          this.applyItemEffect(response);

          // Toast thông báo
          Swal.fire({
            toast: true,
            position: 'top-end',
            icon: 'success',
            title: `${item.icon} ${response.thong_bao}`,
            timer: 2000,
            showConfirmButton: false,
          });
        } else {
          Swal.fire({
            toast: true,
            position: 'top-end',
            icon: 'warning',
            title: response.thong_bao,
            timer: 2000,
            showConfirmButton: false,
          });
        }
      },
      error: (err) => {
        this.itemUsing.set(false);
        Swal.fire('Lỗi', err.error?.message || 'Không thể sử dụng vật phẩm', 'error');
      },
    });
  }

  /**
   * Áp dụng hiệu ứng vật phẩm lên UI
   */
  private applyItemEffect(response: SuDungVatPhamResponse): void {
    const effect = response.hieu_ung;
    if (!effect) return;

    // X2/X3 điểm
    if (effect.he_so_diem && effect.he_so_diem > 1) {
      this.activeMultiplier.set(effect.he_so_diem);
      // Reset sau câu tiếp theo (sẽ reset khi nhận NEW_QUESTION)
    }

    // 50/50 - Loại bỏ đáp án
    if (effect.dap_an_bi_loai && effect.dap_an_bi_loai.length > 0) {
      this.eliminatedOptions.set(effect.dap_an_bi_loai);
    }

    // Khiên bảo vệ
    if (effect.bao_ve_combo) {
      this.hasShield.set(true);
    }

    // Đóng băng thời gian
    if (effect.thoi_gian_them_giay && effect.thoi_gian_them_giay > 0) {
      // Thêm thời gian vào countdown hiện tại
      this.remainingSeconds.update((s) => s + effect.thoi_gian_them_giay!);
    }

    // Hiển thị đáp án đúng
    if (effect.dap_an_dung) {
      Swal.fire({
        title: '👁️ Đáp án đúng',
        html: `<span style="font-size: 3rem; color: #10b981;">${effect.dap_an_dung}</span>`,
        timer: 3000,
        showConfirmButton: false,
      });
    }
  }

  /**
   * Reset hiệu ứng item khi sang câu mới
   */
  private resetItemEffects(): void {
    this.activeMultiplier.set(1);
    this.eliminatedOptions.set([]);
    // Shield giữ nguyên cho đến khi dùng
  }

  /**
   * Kiểm tra đáp án có bị loại không (50/50)
   */
  isOptionEliminated(option: string): boolean {
    return this.eliminatedOptions().includes(option);
  }

  /**
   * Lấy số lượng item theo loại
   */
  getItemCount(loai: LoaiVatPham): number {
    return this.vatPhamService.getItemQuantity(this.inventory(), loai);
  }

  /**
   * Kiểm tra có item không
   */
  hasItemType(loai: LoaiVatPham): boolean {
    return this.getItemCount(loai) > 0;
  }

  // ================== PLAYER INTERACTIONS ==================

  /**
   * Xem profile người chơi (hiện modal)
   */
  viewProfile(userId: number): void {
    this.show_user_modal = true;
    this.user_modal_loading = true;

    // Load user summary
    this.userService.getUserSummary(userId).subscribe({
      next: (res) => {
        this.user_modal_summary = res.data;
        if (this.user_modal_summary) {
          // Tính toán tỉ lệ thắng
          const { so_tran_thang, tong_tran } = this.user_modal_summary;
          this.user_modal_summary.ti_le_thang = tong_tran > 0 ? so_tran_thang / tong_tran : 0;
        }
        this.user_modal_loading = false;
        // Sau khi có summary, load history
        this.loadUserHistory(userId);
      },
      error: () => (this.user_modal_loading = false),
    });
  }

  /**
   * Load lịch sử trận đấu của user (cho modal)
   */
  loadUserHistory(userId: number): void {
    this.user_history_loading = true;
    this.tranDauService.getUserHistory(userId, 0, 5).subscribe({
      next: (res) => {
        this.user_history_items = res.data?.items ?? [];
        this.user_history_loading = false;
      },
      error: () => (this.user_history_loading = false),
    });
  }

  /**
   * Đóng modal user
   */
  closeUserModal(): void {
    this.show_user_modal = false;
    this.user_modal_summary = null;
    this.user_history_items = [];
  }

  /**
   * Map tier label
   */
  mapTierLabel(tier: string | undefined): string {
    switch (tier?.toUpperCase()) {
      case 'BRONZE':
        return 'Đồng';
      case 'SILVER':
        return 'Bạc';
      case 'GOLD':
        return 'Vàng';
      case 'PLATINUM':
        return 'Bạch Kim';
      case 'DIAMOND':
        return 'Kim Cương';
      case 'MASTER':
        return 'Cao Thủ';
      case 'GRANDMASTER':
        return 'Đại Cao Thủ';
      default:
        return 'Tập sự';
    }
  }

  /**
   * Gửi lời mời kết bạn
   */
  sendFriendRequest(userId: number): void {
    Swal.fire({
      title: 'Gửi lời mời kết bạn?',
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Gửi',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.friendService.sendRequest({ target_user_id: userId }).subscribe({
          next: () => {
            Swal.fire({
              toast: true,
              position: 'top-end',
              icon: 'success',
              title: 'Đã gửi lời mời kết bạn!',
              timer: 2000,
              showConfirmButton: false,
            });
          },
          error: (err) => {
            const msg = err.error?.message || 'Không thể gửi lời mời kết bạn';
            Swal.fire({
              toast: true,
              position: 'top-end',
              icon: 'error',
              title: msg,
              timer: 2000,
              showConfirmButton: false,
            });
          },
        });
      }
    });
  }
}
