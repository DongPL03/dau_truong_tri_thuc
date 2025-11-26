import {Component, computed, effect, OnDestroy, OnInit, signal} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule} from '@angular/forms';
import {Base} from '../../base/base';
import {TranDauResponse} from '../../../responses/trandau/trandau-response';
import {SyncStateResponse} from '../../../responses/trandau/syncstate-response';
import {ResponseObject} from '../../../responses/response-object';
import Swal from 'sweetalert2';
import {ThamGiaTranDauDTO} from '../../../dtos/tran-dau/thamgiatrandau-dto';
import {RoiTranDauDTO} from '../../../dtos/tran-dau/roitran-dto';
import {SubmitAnswerDTO} from '../../../dtos/tran-dau/submitanswer-dto';
import {BattleEvent} from '../../../services/ws-trandau.service';
import {environment} from '../../../environments/environment';
import {FinishedPlayer} from '../../../responses/trandau/finished-player';
import {ChatMessage} from '../../../responses/nguoidung/chatmessage';
import {finalize} from 'rxjs/operators';
import {PickerComponent} from '@ctrl/ngx-emoji-mart';
import {UserResponse} from '../../../responses/nguoidung/user-response';

@Component({
  selector: 'app-chi-tiet-phong',
  imports: [CommonModule, FormsModule, PickerComponent],
  templateUrl: './chi-tiet-phong.html',
  styleUrl: './chi-tiet-phong.scss',
  standalone: true
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

  revealedCorrectAnswer = signal<string>('');  // "A" | "B" | "C" | "D" | ''
  revealedExplanation = signal<string>('');    // text giải thích

  // đã nằm trong class ChiTietPhong
  joinedBattle = signal<boolean>(false);


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
        console.warn('⏸️ Bỏ qua effect tick() vì current_question_start chưa hợp lệ:', s.current_question_start);
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

    // @ts-ignore
    this.wsTrandauService.connect(() => token, user.id, id)
      .then(() => {
        console.log('✅ WebSocket connected!');
        this.wsTrandauService.subscribeBattle(id, (ev) => this.handleBattleEvent(ev));
      })
      .catch(err => console.error('❌ WebSocket connect failed:', err));

    // Lấy dữ liệu lần đầu
    this.fetchDetail(id, () => this.doSync());

    [500, 1500, 3000].forEach(time => {
      setTimeout(() => {
        console.log(`🔄 [${time}ms] Đang gọi lại API để check số người...`);
        this.refreshRoomInfo();
      }, time);
    });
    this.currentUserName();
    setTimeout(() => this.syncState.update(s => s ? {...s} : s), 200);
    this.loadUserInfo();
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

  ngOnDestroy() {
    this.clearTimer();
    this.wsTrandauService.disconnect();
  }


// =====================================================
// REST API
// =====================================================

  fetchDetail(id: number, next ?: () => void) {
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
        Swal.fire('Lỗi', 'Không thể tải thông tin phòng', 'error').then(() => this.router.navigateByUrl('/home'));
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
      }
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
    return players.some(p => p.user_id === myId) || this.localJoinedState();
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
    const dto: ThamGiaTranDauDTO = {tran_dau_id: b.id};
    if (!b.cong_khai) dto.ma_pin = this.pinCode();
    this.saving.set(true);
    this.tranDauService.joinBattle(dto as any)
      .pipe(finalize(() => this.saving.set(false)))
      .subscribe({
        next: () => {
          this.joinedBattle.set(true);
          Swal.fire('Thành công', 'Bạn đã tham gia phòng', 'success').then(() => {
          });

          this.localJoinedState.set(true);
          // ⬇️ Sau khi join xong, gọi lại detail để lấy đúng số người tham gia (lúc này DB đã là 2)
          this.refreshRoomInfo();
          this.doSync(); // giữ lại để lấy trạng thái câu hỏi
        },
        error: (e) => {
          // Nếu lỗi là "User đã tham gia", ta coi như thành công
          if (e?.error?.message?.includes('đã tham gia')) {
            Swal.fire('Đã tham gia', 'Bạn đã ở trong phòng này rồi', 'info').then(() => {
            });
            this.refreshRoomInfo();
            this.doSync();
          } else {
            Swal.fire(
              'Không thể tham gia',
              e?.error?.message || 'Vui lòng kiểm tra lại',
              'error'
            ).then(() => {
            });
          }
        }
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
      cancelButtonText: 'Ở lại'
    }).then((r) => {
      if (!r.isConfirmed) return;

      // =========================================================
      // 👇 LOGIC MỚI: KIỂM TRA XEM ĐÃ JOIN CHƯA
      // =========================================================

      // TRƯỜNG HỢP 1: Chưa tham gia (đang xem) -> Chỉ cần chuyển trang về Home
      if (!this.isJoined()) {
        this.wsTrandauService.disconnect(); // Ngắt kết nối socket cho sạch
        this.router.navigateByUrl('/home').then(r => {
        });
        return; // Dừng hàm tại đây, không gọi API bên dưới
      }

      // TRƯỜNG HỢP 2: Đã tham gia -> Gọi API để Backend xóa tên khỏi danh sách
      const b = this.battle();
      if (!b) return;

      const dto: RoiTranDauDTO = {tran_dau_id: b.id};
      this.saving.set(true);

      this.tranDauService.leaveBattle(dto as any).subscribe({
        next: () => {
          this.saving.set(false);
          Swal.fire('Đã rời phòng', '', 'success').then(() => {
          });
          this.wsTrandauService.disconnect();
          this.router.navigateByUrl('/home').then(r => {
          });
        },
        error: (e) => {
          this.saving.set(false);
          // Dù lỗi API (do mạng lag hay gì đó) thì cũng nên cho người dùng thoát ra
          // Nếu muốn chặt chẽ thì giữ alert, nếu muốn UX mượt thì navigate luôn
          Swal.fire('Lỗi', e?.error?.message || 'Không thể rời phòng', 'error').then(r => {
          });

          // Option: Nếu API lỗi "Bạn chưa ở trong phòng", ta vẫn cho họ về Home luôn
          if (e?.error?.message?.includes('chưa ở trong phòng')) {
            this.router.navigateByUrl('/home').then(r => {
            });
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
        this.leaderboard.update(list =>
          list.map(p =>
            p.user_id === evt.user_id ? {...p, da_roi: false} : p
          )
        );
        Swal.fire('👋 Người chơi mới', `${evt.ho_ten} vừa tham gia phòng`, 'info').then(r => {
        });
        this.refreshRoomInfo();
        break;
      }
      case 'PLAYER_LEFT': {
        // Đánh dấu "đã rời trận" trên leaderboard
        this.leaderboard.update(list =>
          list.map(p =>
            p.user_id === evt.user_id ? {...p, da_roi: true} : p
          )
        );
        Swal.fire('🚪 Người chơi rời đi', `${evt.ho_ten} đã rời phòng`, 'warning').then(r => {
        });
        this.refreshRoomInfo();
        break;
      }
      case 'BATTLE_STARTED':
        Swal.fire({
          icon: 'success',
          title: 'Trận đấu bắt đầu!',
          text: `Phòng: ${evt.ten_phong} (${evt.tong_cau_hoi} câu hỏi, ${evt.thoi_gian_moi_cau_giay}s mỗi câu)`,
          timer: 1800,
          showConfirmButton: false
        }).then(r => {
        });

        this.battle.update((b) => ({
          ...b!,
          trang_thai: 'ONGOING',
          bat_dau_luc: evt.bat_dau_luc
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
            }
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

        this.syncState.set({...newState});

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
        this.revealedCorrectAnswer.set(evt.dap_an_dung);      // "A" | "B" | "C" | "D"
        this.revealedExplanation.set(evt.giai_thich || '');
        break;
      }
      case 'SCORE_UPDATE':
        const myId = this.userService.getUserId();
        if (evt.user_id !== myId) {
          // Người khác được cập nhật điểm => chỉ cập nhật leaderboard, không popup
          console.log(`📡 SCORE_UPDATE từ người khác (${evt.ho_ten}), bỏ qua popup.`);
          // this.toastService.show(`${evt.ho_ten} đã nộp đáp án`, {type: 'info'});
          return;
        }

        Swal.fire(
          evt.correct ? '✅ Chính xác!' : '❌ Sai mất rồi',
          `+${evt.gained_points} điểm`,
          evt.correct ? 'success' : 'error'
        ).then(() => {
        });

        setTimeout(() => {
          this.syncState.update(s => s ? {...s, my_total_points: evt.total_points} : s);
        }, 300);
        break;


      case 'LEADERBOARD_UPDATE':
        // 🏅 Cập nhật leaderboard
        // @ts-ignore
        this.leaderboard.set(evt.players || []);
        if (evt.players && evt.players.length > 0) {
          this.onlineCount.set(evt.players.length);
        }
        break;


      case 'FINISHED': {
        console.log('🏁 Trận đấu kết thúc', evt);
        this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'FINISHED'});

        const myId = this.userService.getUserId();

        // 🧮 Lưu kết quả tạm để hiển thị ở màn hình summary
        this.finalResult = {
          winner: evt.winner,
          leaderboard: evt.leaderboard as FinishedPlayer[],
          myId,
        };

        // tìm dòng của chính mình
        this.mySummaryRow = this.finalResult.leaderboard.find(p => p.user_id === myId);
        this.isWinnerMe = !!(this.finalResult.winner && this.finalResult.winner.user_id === myId);


        // 🧭 Chuyển trạng thái sang summary view
        this.showSummary.set(true);
        this.clearTimer();
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

        this.chatMessages.update(list => [...list, msg]);

        // auto scroll xuống cuối
        setTimeout(() => {
          const box = document.querySelector('.chat-messages');
          if (box) {
            (box as HTMLElement).scrollTop = (box as HTMLElement).scrollHeight;
          }
        }, 50);

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
        this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'ONGOING'});
        Swal.fire('Bắt đầu!', 'Trận đấu đã bắt đầu', 'success').then(r => {
        });
        this.doSync();
      },
      error: (e) => {
        this.saving.set(false);
        Swal.fire('Không thể bắt đầu', e?.error?.message || 'Bạn có quyền chủ phòng?', 'error').then(r => {
        });
      }
    });
  }


  finishBattle() {
    const id = this.battle()?.id;
    if (!id) return;
    Swal.fire({
      title: 'Kết thúc trận?',
      text: 'Hệ thống sẽ chốt điểm & phát kết quả',
      icon: 'warning',
      showCancelButton: true
    }).then(r => {
      if (!r.isConfirmed) return;
      this.saving.set(true);
      this.tranDauService.finishBattle(id).subscribe({
        next: () => {
          this.saving.set(false);
          this.battle.set({...(this.battle() as TranDauResponse), trang_thai: 'FINISHED'});
          Swal.fire('Đã kết thúc', 'Xem bảng xếp hạng ở phía dưới', 'success').then(r => {
          });
          this.doSync();
        },
        error: (e) => {
          this.saving.set(false);
          Swal.fire('Không thể kết thúc', e?.error?.message || 'Thử lại sau', 'error').then(r => {
          });
        }
      });
    });
  }

  submitSelectedAnswer() {
    const s = this.syncState();
    const b = this.battle();
    if (!s || !b || s.current_question_index < 0 || !s.current_question_id) return;

    // ⛔ đã nộp câu hiện tại rồi
    if (this.submittedCurrentAnswer()) {
      Swal.fire('Bạn đã nộp đáp án', 'Hãy chờ câu hỏi tiếp theo nhé', 'info').then(() => {
      });
      return;
    }

    const ans = this.selectedAnswer();
    if (!ans) {
      Swal.fire('Chưa chọn đáp án', 'Hãy chọn A/B/C/D', 'info').then(r => {
      });
      return;
    }

    const dto: SubmitAnswerDTO = {tran_dau_id: b.id, cau_hoi_id: s.current_question_id, answer: ans};
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
        Swal.fire('Không thể nộp đáp án', e?.error?.message || 'Thử lại sau', 'error').then(r => {
        });
      }
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
    this.tranDauService.sendChat({tran_dau_id: battleId, noi_dung: content} as any)
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
            timer: 3000
          }).then(r => {
          });
        }
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
      error: (err) => console.error('Lỗi refresh room info', err)
    });
  }

// Hàm toggle
  toggleEmojiPicker() {
    this.showEmojiPicker.update(v => !v);
  }

  // Hàm xử lý khi chọn 1 emoji
  addEmoji(event: any) {
    const emoji = event.emoji.native;
    // Nối emoji vào chuỗi input hiện tại
    this.chatInput.update(current => current + emoji);
    // (Tùy chọn) Đóng bảng sau khi chọn xong nếu muốn
    // this.showEmojiPicker.set(false);
  }

  // Trong class ChiTietPhong

  getAvatarColor(name: string): string {
    const colors = [
      '#ef4444', '#f97316', '#f59e0b', '#84cc16',
      '#10b981', '#06b6d4', '#3b82f6', '#6366f1',
      '#8b5cf6', '#d946ef', '#f43f5e'
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
    this.router.navigateByUrl('/tran-dau/pending').then(r => {
    }); // hoặc '/battle/danh-sach-bo-cau-hoi'
  }

  goBackHome() {
    this.router.navigateByUrl('/home').then(r => {
    });
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

    this.router.navigate(['/luyen-tap'], {
      queryParams: {
        bo_cau_hoi_id: boId
      }
    }).then(r => {
    });
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
    return (s.a || '').length > threshold ||
      (s.b || '').length > threshold ||
      (s.c || '').length > threshold ||
      (s.d || '').length > threshold;
  });
}
